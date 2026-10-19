pub fn main() -> Result<(), exec::Error> {
    const OUT_DIR: &str = env!("OUT_DIR");
    let path = format!("{OUT_DIR}/bin/wasm-bindgen-test-runner");
    Err(exec::execvp(path, std::env::args()))
}
