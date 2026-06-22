set positional-arguments

[working-directory: 'integration']
test-integrations toolchain="":
    cargo ${1:+"$1"} test --target wasm32-unknown-unknown --workspace
    cargo ${1:+"$1"} test --workspace
    XCARGO_REPORT_TAG=opt cargo ${1:+"$1"} test --target wasm32-unknown-unknown --workspace --config profile.test.opt-level=3
    XCARGO_REPORT_TAG=release cargo ${1:+"$1"} test --target wasm32-unknown-unknown --workspace

test-all-integrations: (test-integrations "+stable") (test-integrations "+nightly") (test-integrations "+1.84")

test-cli:
    cargo test -p wasm_split_helpers
    cargo test -p wasm_split_cli_support --all-features

all-tests: test-all-integrations test-cli

fmt-check:
    cargo fmt --all --check
    cd integration && cargo fmt --all --check
    cd test-runner && cargo fmt --all --check

clippy:
    cargo +nightly clippy -- -Dwarnings
    cd integration && cargo +nightly clippy -- -Dwarnings
    cd test-runner && cargo +nightly clippy -- -Dwarnings

semver-check:
    cargo semver-checks

[private]
@clean-dir:
    git diff --quiet HEAD || (echo >&2 "not in a clean working directory" && exit 1)

release-checks: clean-dir fmt-check clippy all-tests semver-check
pr-checks: clean-dir fmt-check clippy all-tests
