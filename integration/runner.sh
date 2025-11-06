#!/usr/bin/env bash
THIS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd -- "$THIS_DIR"/../test-runner || exit 1
unset RUSTUP_TOOLCHAIN CARGO # make sure the below invocation is done with the toolchain in test-runner
export XCARGO_BIN_NAME="$CARGO_BIN_NAME"
export XCARGO_MANIFEST_DIR="$CARGO_MANIFEST_DIR"
export XCARGO_TARGET_TMPDIR="$CARGO_TARGET_TMPDIR"
cargo run --release -p wasm-split-test-runner -- "$@"
