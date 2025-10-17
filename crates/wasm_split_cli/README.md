# `wasm_split_cli_support`

This crate provides a library and cli to split a WebAssembly (WASM) module into multiple parts, at split point indicated by the accompanying `wasm_split_helpers` crate.

This crate was adapted from an original prototype, which you can find [here](https://github.com/jbms/wasm-split-prototype), with an in-depth description of the approach [here](https://github.com/rustwasm/wasm-bindgen/issues/3939).

## Executable cli

This crate includes a binary cli version. To install run

```bash
cargo install --locked wasm_split_cli_support --features="build-binary"
```
