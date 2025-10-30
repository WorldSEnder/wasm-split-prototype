#!/usr/bin/env bash
THIS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd -- "$THIS_DIR"/../test-runner || exit 1
unset RUSTUP_TOOLCHAIN CARGO # make sure the below invocation is done with the toolchain in test-runner
cargo run -p wasm-split-test-runner -- "$@"
