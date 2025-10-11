#!/bin/env bash
THIS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
cd -- "$THIS_DIR"/../test-runner || exit 1
cargo run -p wasm-split-test-runner -- "$@"
