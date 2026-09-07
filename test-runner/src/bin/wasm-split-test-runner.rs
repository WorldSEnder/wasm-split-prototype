use eyre::{Result, ensure};
use serde::Serialize;
use sha2::Digest;
use std::{
    collections::HashMap,
    env::args_os,
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, Instant},
};
use tracing::{
    Subscriber,
    field::{Field, Visit},
    span,
};
use tracing_subscriber::{layer::Context, registry::LookupSpan};

#[serde_with::serde_as]
#[derive(Default, serde::Serialize)]
struct Report {
    #[serde_as(as = "Vec<(_, _)>")]
    file_sizes: HashMap<PathBuf, u64>,
    #[serde_as(as = "Vec<(_, _)>")]
    file_hashes: HashMap<PathBuf, ContentHash>,
    cli_runtime: Duration,
}

fn print_report(report: &Report, report_dir: &Path) -> Result<()> {
    let branded_report_name = format!(
        "report-{}{}.json",
        std::env::var("XRUSTUP_TOOLCHAIN").unwrap(),
        if let Ok(tag) = std::env::var("XCARGO_REPORT_TAG") {
            format!("-{tag}")
        } else {
            String::new()
        },
    );
    let report_path = report_dir.join(branded_report_name);
    let mut report_file = std::fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&report_path)
        .expect("report path to open");
    serde_json::to_writer_pretty(&mut report_file, &report)?;
    Ok(())
}

fn wasm_bindgen_test_runner() -> Command {
    Command::new(
        option_env!("CARGO_BIN_FILE_WASM_BINDGEN_CLI_wasm-bindgen-test-runner")
            .unwrap_or("wasm-bindgen-test-runner"),
    )
}

#[derive(PartialEq, Eq, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
struct ContentHash([u8; 32]);
impl std::fmt::Debug for ContentHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for d in &self.0 {
            write!(f, "{:02x}", d)?;
        }
        Ok(())
    }
}

fn hash_file(path: &Path) -> Result<ContentHash> {
    struct Sink(sha2::Sha256);
    impl std::io::Write for Sink {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.update(buf);
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
    let mut sink = Sink(sha2::Sha256::new());
    let _written = std::io::copy(&mut std::fs::File::open(path)?, &mut sink)?;
    Ok(ContentHash(sink.0.finalize().0))
}

fn collect_file_sizes(
    report: &mut Report,
    main_file: &Path,
    split: &wasm_split_cli_support::SplitWasm,
) -> Result<()> {
    for module in split
        .split_modules
        .iter()
        .map(|p| p.as_ref())
        .chain([main_file])
    {
        let meta = std::fs::File::open(module)?.metadata()?;
        let file_hash = hash_file(module)?;
        let file_size = meta.len();
        report.file_sizes.insert(module.to_path_buf(), file_size);
        report.file_hashes.insert(module.to_path_buf(), file_hash);
    }
    Ok(())
}

fn check_reproducible(first_report: &Report, second_report: &Report) -> Result<()> {
    ensure!(
        first_report.file_hashes == second_report.file_hashes,
        "mismatching file hashes/sizes. Expected `left` but got `right`\n  left-sizes = {:#?}\n  left-hashes = {:#?}\n  right-sizes = {:#?}\n  right-hashes = {:#?}",
        first_report.file_sizes,
        first_report.file_hashes,
        second_report.file_sizes,
        second_report.file_hashes,
    );
    Ok(())
}

fn with_perf_tracing<R>(report_dir: &Path, f: impl FnOnce() -> R) -> R {
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::{Layer, filter::FilterFn};

    let fmt = tracing_subscriber::fmt::layer();
    let fmt = fmt.with_filter(tracing_subscriber::EnvFilter::from_default_env());
    let perf = tracing_chrome::ChromeLayerBuilder::new();
    let report_path = report_dir.join(format!(
        "trace-{}.json",
        std::time::SystemTime::UNIX_EPOCH
            .elapsed()
            .unwrap()
            .as_micros()
    ));
    const PERF_KEY_NAME: &str = "perf_key";
    struct Name(Field, String);
    impl Visit for Name {
        fn record_debug(&mut self, field: &Field, value: &dyn core::fmt::Debug) {
            use std::fmt::Write;
            if &self.0 == field {
                let _ = write!(&mut self.1, "{value:?}");
            }
        }
    }
    struct SpanNamePerfKey;
    struct NameExtension(String);
    impl<S: Subscriber + for<'lookup> LookupSpan<'lookup>> Layer<S> for SpanNamePerfKey {
        fn on_new_span(&self, attrs: &span::Attributes<'_>, id: &span::Id, ctx: Context<'_, S>) {
            if let Some(perf_key) = attrs.fields().field(PERF_KEY_NAME) {
                let mut name = Name(perf_key, String::new());
                attrs.record(&mut name);
                let name = name.1;
                ctx.span(id)
                    .unwrap()
                    .extensions_mut()
                    .insert(NameExtension(name));
            }
        }
    }
    let perf = perf.file(report_path);
    let perf = perf.include_args(true);
    let perf = perf.name_fn(Box::new(|data| match *data {
        tracing_chrome::EventOrSpan::Event(event) => {
            if let Some(perf_key) = event.fields().find(|field| field.name() == PERF_KEY_NAME) {
                let mut name = Name(perf_key, String::new());
                event.record(&mut name);
                name.1
            } else {
                event.metadata().name().to_string()
            }
        }
        tracing_chrome::EventOrSpan::Span(span_ref) => span_ref
            .extensions()
            .get::<NameExtension>()
            .map(|ext| &ext.0)
            .cloned()
            .unwrap_or_else(|| span_ref.metadata().name().to_string()),
    }));
    let (perf, perf_guard) = perf.build();
    let perf = SpanNamePerfKey.and_then(perf);
    let perf = perf.with_filter(FilterFn::new(|metadata| {
        metadata.fields().field(PERF_KEY_NAME).is_some()
    }));
    let subscriber = tracing_subscriber::registry().with(fmt).with(perf);
    let dispatch = tracing::Dispatch::new(subscriber);
    let r = tracing::dispatcher::with_default(&dispatch, f);
    let _ = perf_guard;
    r
}

fn wasm_split_cli(target: &Path, dir: &Path, report_dir: &Path) -> Result<(PathBuf, Report)> {
    let main_file = dir.join("main.wasm");
    let input = std::fs::read(target)?;
    let mut report: Report = Report::default();
    let start_time = Instant::now();

    let split = with_perf_tracing(report_dir, || {
        wasm_split_cli_support::transform({
            let mut split_opts = wasm_split_cli_support::Options::new(&input);
            split_opts.output_dir = dir;
            split_opts.main_out_path = &main_file;
            split_opts.main_module = "./wasm-bindgen-test";
            split_opts.verbose = true;
            split_opts.strict_tests = true;
            split_opts.emit_dwarf = true;
            split_opts
        })
    })?;
    let time_taken = Instant::now().duration_since(start_time);
    report.cli_runtime = time_taken;

    collect_file_sizes(&mut report, &main_file, &split)?;

    Ok((main_file, report))
}

fn find_build_tempdir_root(target: &Path) -> PathBuf {
    let mut candidate = target;
    let build_dir = loop {
        let parent = candidate.parent().expect("to find a cachedir tag");
        if parent.join("CACHEDIR.TAG").exists() {
            break parent;
        }
        candidate = parent;
    };
    let out_path = build_dir
        .join("wasm-split-integration")
        .join(std::env::var_os("XCARGO_PKG_NAME").expect("pkg name set by runner script"));
    std::fs::create_dir_all(&out_path).expect("make temp dir host");
    out_path
}

pub fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let mut args = args_os();
    let _ = args.next().expect("args[0] to be the name of this runner");
    let target = args.next().expect("args[1] to be a wasm program to test");
    let target_manifest_dir = std::env::var_os("XCARGO_MANIFEST_DIR")
        .expect("env variable to manifest should be set by runner script");

    let target = Path::new(&target);
    let target_report_dir = find_build_tempdir_root(target);
    let mut tempdir = tempfile::Builder::new().tempdir_in(&target_report_dir)?;
    tempdir.disable_cleanup(true); // keep the dir for debugging
    eprintln!(
        "Splitting wasm from {} in {}",
        target_manifest_dir.display(),
        tempdir.path().display()
    );

    let (split_main, report) = wasm_split_cli(target, tempdir.path(), &target_report_dir)?;
    print_report(&report, &target_report_dir)?;
    if !std::env::var_os("XTEST_SKIP_REPRODUCTION").is_some_and(|skip| !skip.is_empty()) {
        // check that the result is reproducible.
        // we could do this in its own directory. However, when the result is reproducible,
        // the second output should not have overwritten anything from the first either way.
        let (_, second_report) = wasm_split_cli(target, tempdir.path(), &target_report_dir)?;
        check_reproducible(&report, &second_report)?;
    }

    let mut wbg = wasm_bindgen_test_runner();
    // Currently, testing is ONLY supported in browser mode. For node and others, the wrapper script needs to be
    // modified and the setup reworked for the test runner to pick up our generated wasm modules.
    wbg.env("WASM_BINDGEN_USE_BROWSER", "1");
    wbg.env("WASM_BINDGEN_KEEP_LLD_EXPORTS", "1");
    // We cd to the temp dir where we generated the split to serve these as "fallback" when the runner inevitably
    // doesn't find these file in its own temp dir
    wbg.current_dir(&tempdir);

    wbg.arg(&split_main).args(args);
    let wbg_exit = wbg.status()?;
    ensure!(
        wbg_exit.success(),
        "Failed to execute wasm-bindgen-test-runner"
    );

    tempdir.disable_cleanup(false);
    Ok(())
}
