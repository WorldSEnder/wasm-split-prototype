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
    cargo clippy -- -Dwarnings
    cd integration && cargo clippy -- -Dwarnings
    cd test-runner && cargo clippy -- -Dwarnings

semver-checks:
    cargo semver-checks

release-checks: fmt-check test-all-integrations semver-checks
pr-checks: fmt-check test-all-integrations
