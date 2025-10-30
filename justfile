set positional-arguments

[working-directory: 'integration']
test-integrations toolchain="":
    cargo ${1:+"$1"} test --target wasm32-unknown-unknown --workspace
    cargo ${1:+"$1"} test --workspace
    cargo ${1:+"$1"} test --target wasm32-unknown-unknown --config profile.test.opt-level=3

test-all-integrations: (test-integrations "+stable") (test-integrations "+nightly") (test-integrations "+1.84")

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

release-checks: fmt-check clippy test-all-integrations semver-check
pr-checks: fmt-check clippy test-all-integrations
