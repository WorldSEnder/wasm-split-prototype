set positional-arguments

[working-directory: 'integration']
test-integrations toolchain="":
    xvfb-run cargo ${1:+"$1"} test --target wasm32-unknown-unknown --workspace
    cargo ${1:+"$1"} test --workspace
    xvfb-run cargo ${1:+"$1"} test --target wasm32-unknown-unknown --config profile.test.opt-level=3

test-all-integrations: (test-integrations "+stable") (test-integrations "+nightly") (test-integrations "+1.84")

semver-checks:
    cargo semver-checks

release-checks: test-all-integrations semver-checks
