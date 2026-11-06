#!/usr/bin/env bash
THIS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd -- "$THIS_DIR"/../test-runner || exit 1
export XRUSTUP_TOOLCHAIN="$RUSTUP_TOOLCHAIN"
export XCARGO_BIN_NAME="$CARGO_BIN_NAME"
export XCARGO_MANIFEST_PATH="$CARGO_MANIFEST_PATH"
export XCARGO_TARGET_TMPDIR="$CARGO_TARGET_TMPDIR"
export XCARGO_TARGET_DIR="$CARGO_TARGET_DIR"
export XCARGO_PKG_NAME="$CARGO_PKG_NAME"
unset RUSTUP_TOOLCHAIN CARGO CARGO_TARGET_DIR OUT_DIR # make sure the below invocation is done with the toolchain in test-runner
cargo run --release -p wasm-split-test-runner -- "$@"
