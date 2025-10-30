# Contributing

Contributions are welcome! Feel free to open an issue to clarify questions that are not resolved in this guide.

The repository uses [justfiles](https://github.com/casey/just) to automate some common tasks.

## Testing

Integration testing requires a driver for a browser to be installed. `wasm-pack` can automatically download
these if you have the tool installed, otherwise you need to check relevant docs. `wasm-pack` will fetch
the tools to a path such as `CHROMEDRIVER=~/.cache/.wasm-pack/chromedriver-<hash>/chromedriver`. Set this as
an environment variable (other browsers are also supported via the env variables like `GECKODRIVER`,
`SAFARIDRIVER`; `wasm-bindgen-test-runner` will print a helpful advice if none is found).

Finally, run the command

```bash
just pr-checks
```

## Releasing

We use [`cargo-semver-checks`] to check for semver incompatibilities when releasing a new update. Run with

```bash
just release-checks
```

This tool does _NOT_ consider proc-macro crates hence you should take additional care here. Check the comment
in the workspace `Cargo.toml` for some of the subtleties.

[`cargo-semver-checks`]: https://crates.io/crates/cargo-semver-checks

## I don't know what to hack on

Look for tracking issues with the <span style="vertical-align: middle;"><img src="https://img.shields.io/badge/future--version-85ec07?style=flat"></span> tag.
