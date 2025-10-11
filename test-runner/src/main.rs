use eyre::{Result, bail};
use std::{
    env::args_os,
    path::{Path, PathBuf},
    process::Command,
};

fn wasm_bindgen_test_runner() -> Command {
    Command::new(
        option_env!("CARGO_BIN_FILE_WASM_BINDGEN_CLI_wasm-bindgen-test-runner")
            .unwrap_or("wasm-bindgen-test-runner"),
    )
}

fn wasm_split_cli(target: &Path, dir: &Path) -> Result<PathBuf> {
    let main_file = dir.join("main.wasm");
    let input = std::fs::read(target)?;

    let _split = wasm_split_cli_support::transform(wasm_split_cli_support::Options {
        input_wasm: &input,
        output_dir: dir,
        main_out_path: &main_file,
        verbose: true,
        main_module: "./wasm-bindgen-test",
        link_name: Path::new("__wasm_split.js"),
    })?;
    Ok(main_file)
}

pub fn main() -> Result<()> {
    let mut args = args_os();
    let _ = args.next().expect("args[0] to be the name of this runner");
    let target = args.next().expect("args[1] to be a wasm program to test");

    let target = Path::new(&target);
    let mut tempdir = tempfile::tempdir()?;
    tempdir.disable_cleanup(true); // keep the dir for debugging
    println!("Splitting wasm in {}", tempdir.path().display());

    let split_main = wasm_split_cli(target, tempdir.path())?;

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
