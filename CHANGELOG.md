## wasm_split_helpers v0.2.3

- Propagate `debug_assertions` to the CLI with a new marker in the hidden `#[link_section]`.

## wasm_split_cli v0.2.2

- Add a canary import to diagnose modules getting loaded from different compilation units.
  This is enabled when the crate is compiled with `debug_assertions` enabled. Detection
  of this is automatic when compiling against `wasm_split_helpers >= 0.2.3`. (#40).
- Fixed an error on nightly Rust related to zero-sized symbols past the defined data
  segments (#41).
- Fix: globally exposed data symbols are now exported with their relocated address (#42).

## wasm_split_helpers v0.2.2

- A failed split-module load is no longer cached for the lifetime of the session
  (one transient network failure used to permanently break the module; see #35).
  Only successful loads are now memoized, so a later load attempt starts fresh.
  `ensure_loaded` returns `Result<(), SplitLoaderError>`, and `#[wasm_split(..)]`
  gains a `fallible` option that makes the generated wrapper (and its `preload`)
  return the load error instead of panicking. Without `fallible` the wrapper and
  `preload` keep their previous infallible signatures and a hard failure panics,
  as before.

## wasm_split_helpers v0.2.1

- Improved resilience against multiple versions of the lib being included, mainly working around akward usage of `#[link_section]`
  on wasm targets. See also [rust-lang/rust#56639](https://github.com/rust-lang/rust/issues/56639).
- Changed linkage in the macro in response to [rust-lang/rust#149868](https://github.com/rust-lang/rust/pull/149868).
  See #27 for reference, thanks @EvanCarroll.

## wasm_split_cli v0.2.1

- Improved error messages.
- Work around an issue with wasm-ld and LTO which does not generate relocations for all stubs, see #29.
  Fixed in #30 by @adrianncovaci.

## wasm_split_cli v0.2.0, wasm_split_helpers v0.2.0

- Add a custom section to track which version of the runtime/macro was used to produce the binary before transforming
  in the CLI. This can prevent runtime/CLI mismatch.

- The link module is now entirely handled by the CLI, the macro merely emits a placeholder.

## wasm_split_cli v0.2.0-rc.2, wasm_split_helpers v0.2.0-rc.2

- Fix some semver hazards before the release

## wasm_split_cli v0.2.0-rc.1

- Data symbols will now get merged if they overlap in the input.
  This leads to smaller files and should put as much data as possible into the splits instead of the main module.

## wasm_split_helpers v0.2.0-rc.1

- On non-wasm targets, the split function will now forward directly to the definition, so that functionality is preserved.
  On wasm targets (`target_family = "wasm"`), the cli must be run on the generated wasm object!

- Testing has been integrated by running `wasm-bindgen-test-runner`. See the `integration` directory.

## v0.2.0-rc.0

- Includes splitting of data symbols and a stronger dependency analysis
