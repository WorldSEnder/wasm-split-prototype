pub fn main() -> Result<(), exec::Error> {
    Err(exec::execvp(
        env!("CARGO_BIN_FILE_WASM_BINDGEN_CLI_wasm-bindgen"),
        std::env::args(),
    ))
}
