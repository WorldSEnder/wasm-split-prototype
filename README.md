<div align="center">
<h1>Wasm split</h1>
<h3>Runtime/macro</h3>
<p>
<a href="https://crates.io/crates/wasm_split_helpers">
<img src="https://img.shields.io/crates/v/wasm_split_helpers.svg?style=flat-square" alt="Crates.io version" />
<img src="https://img.shields.io/crates/d/wasm_split_helpers.svg?style=flat-square" alt="Download" />
</a>
<a href="https://docs.rs/wasm_split_helpers"><img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square" alt="docs.rs docs" /></a>
</p>
<h3>CLI</h3>
<p>
<a href="https://crates.io/crates/wasm_split_cli_support"><img src="https://img.shields.io/crates/v/wasm_split_cli_support.svg?style=flat-square" alt="Crates.io version" /></a>
</p>
</div>

This collection of crates allow you to indicate that certain functions are appropriate split points for lazy-loaded code in WebAssembly (WASM).
A single input wasm module is then split into multiple modules that are fetched as needed, when a function in another module is called.

## What this is not

This is not a dynamic linker for wasm! Runtime loading expects to receive _exactly_ the modules that have been produced by splitting before.
Selectively updating only specific files after recompiling is not supported. On the technical side, this is because a reachability analysis
is run on the whole artifact which might migrate code from one module to another due to unrelated changes.

## History

The tool originated out of [a prototype](https://github.com/jbms/wasm-split-prototype) written by @jbms. It has then received updates from
the leptos maintainers and finally migrated into a stand-alone crate to serve general build tools.
