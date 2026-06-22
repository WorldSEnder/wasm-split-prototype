use eyre::{Result, bail};
use std::{
    collections::HashMap,
    env::args_os,
    path::{Path, PathBuf},
    process::Command,
    time::{Duration, Instant},
};

#[serde_with::serde_as]
#[derive(Default, serde::Serialize)]
struct Report {
    #[serde_as(as = "Vec<(_, _)>")]
    file_sizes: HashMap<PathBuf, u64>,
    cli_runtime: Duration,
}

fn print_report(report: Report, report_dir: &Path) -> Result<()> {
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
        let file_size = std::fs::File::open(module)?.metadata()?.len();
        report.file_sizes.insert(module.to_path_buf(), file_size);
    }
    Ok(())
}

fn wasm_split_cli(target: &Path, dir: &Path) -> Result<(PathBuf, Report)> {
    let main_file = dir.join("main.wasm");
    let input = std::fs::read(target)?;
    let mut report: Report = Report::default();
    let start_time = Instant::now();

    let split = wasm_split_cli_support::transform({
        let mut split_opts = wasm_split_cli_support::Options::new(&input);
        split_opts.output_dir = dir;
        split_opts.main_out_path = &main_file;
        split_opts.main_module = "./wasm-bindgen-test";
        split_opts.verbose = true;
        split_opts.strict_tests = true;
        split_opts
    })?;
    let time_taken = Instant::now().duration_since(start_time);
    report.cli_runtime = time_taken;

    collect_file_sizes(&mut report, &main_file, &split)?;

    Ok((main_file, report))
}

fn find_build_tempdir_root(target: &Path) -> PathBuf {
    let build_dir = target
        .parent()
        .expect("strip executable name")
        .parent()
        .expect("strip profile name");
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
    let target_manifest_dir = std::env::var("XCARGO_MANIFEST_DIR")
        .expect("env variable to manifest should be set by runner script");

    let target = Path::new(&target);
    let target_report_dir = find_build_tempdir_root(target);
    let mut tempdir = tempfile::Builder::new().tempdir_in(&target_report_dir)?;
    tempdir.disable_cleanup(true); // keep the dir for debugging
    eprintln!(
        "Splitting wasm from {target_manifest_dir} in {}",
        tempdir.path().display()
    );

    let (split_main, report) = wasm_split_cli(target, tempdir.path())?;
    print_report(report, &target_report_dir)?;

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
    if !wbg_exit.success() {
        bail!("Failed to execute wasm-bindgen-test-runner");
    }

    tempdir.disable_cleanup(false);
    Ok(())
}
