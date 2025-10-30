set positional-arguments

[working-directory: 'integration']
test-integrations toolchain="":
    cargo ${1:+"$1"} test --target wasm32-unknown-unknown --workspace
    cargo ${1:+"$1"} test --workspace
    cargo ${1:+"$1"} test --target wasm32-unknown-unknown --config profile.test.opt-level=3

test-all-integrations: (test-integrations "+stable") (test-integrations "+nightly") (test-integrations "+1.84")

semver-checks:
    cargo semver-checks

release-checks: test-all-integrations semver-checks
