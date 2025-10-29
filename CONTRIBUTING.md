# Contributing

Contributions are welcome! Feel free to open an issue to clarify questions that are not resolved in this guide.

## Testing

Integration testing requires a driver for a browser to be installed. `wasm-pack` can automatically download
these if you have the tool installed, otherwise you need to check relevant docs. `wasm-pack` will fetch
the tools to a path such as `CHROMEDRIVER=~/.cache/.wasm-pack/chromedriver-<hash>/chromedriver`. Set this as
an environment variable (other browsers are also supported via the env variables like `GECKODRIVER`,
`SAFARIDRIVER`; `wasm-bindgen-test-runner` will print a helpful advice if none is found).

Finally, run the commands below where `toolchain` is all of `1.84` (MSRV), `stable` and `nightly`

```bash
cd integration
# tests should work for both wasm and non-wasm targets for stable cargo
cargo "+$toolchain" test --workspace
cargo "+$toolchain" test --target wasm32-unknown-unknown --workspace
# enabling optimizations puts additional pressure on the cli to split correctly
cargo "+$toolchain" test --target wasm32-unknown-unknown --config profile.test.opt-level=3 --workspace
```

## Releasing

We use [`cargo-semver-checks`] to check for semver incompatibilities when releasing a new update. Run with

```bash
cargo semver-checks
```

This tool does _NOT_ consider proc-macro crates hence you should take additional care here. Check the comment
in the workspace `Cargo.toml` for some of the subtleties.

[`cargo-semver-checks`]: https://crates.io/crates/cargo-semver-checks

## I don't know what to hack on

Look for tracking issues with the <span style="vertical-align: middle;"><img src="https://img.shields.io/badge/future--version-85ec07?style=flat"></span> tag.
