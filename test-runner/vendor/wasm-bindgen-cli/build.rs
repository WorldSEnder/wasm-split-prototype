use std::process::Command;

fn cargo_install_cmd() -> Command {
    let has_binstall = Command::new("cargo-binstall").args(["-V"]).output();
    if let Ok(out) = has_binstall {
        println!(
            "Found cargo-binstall {}",
            String::from_utf8_lossy_owned(out.stdout)
        );
        let mut cmd = Command::new("cargo-binstall");
        cmd.args(["--no-confirm", "--locked", "--no-track", "--force"]);
        cmd
    } else {
        let mut cmd = Command::new("cargo");
        cmd.args(["install", "--no-track", "--locked", "--force"]);
        cmd
    }
}

fn main() {
    let v = env!("CARGO_PKG_VERSION");
    let out_dir = std::env::var("OUT_DIR").unwrap();
    println!("cargo::rerun-if-env-changed=OUT_DIR");
    println!("installing into {out_dir}");

    let exit = cargo_install_cmd()
        .args(["wasm-bindgen-cli", "--version", v, "--root", &out_dir])
        .output()
        .unwrap();
    if !exit.status.success() {
        use std::io::Write;
        let _ = std::io::stderr().write(&exit.stderr);
        panic!("install command didn't run successfully");
    }
}
