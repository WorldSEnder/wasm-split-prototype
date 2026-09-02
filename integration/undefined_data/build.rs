fn main() {
    // rustc stopped passing `--allow-undefined` to wasm-ld
    // (rust-lang/rust#149868). Workflows that still pass it themselves get
    // modules in which an undefined data symbol links to address 0 instead
    // of failing the link; this fixture opts in to produce that shape.
    if std::env::var("CARGO_CFG_TARGET_FAMILY").as_deref() == Ok("wasm") {
        println!("cargo:rustc-link-arg=--allow-undefined");
    }
}
