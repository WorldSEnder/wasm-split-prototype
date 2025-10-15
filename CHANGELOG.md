## 0.2.0-rc.2

- Data symbols will now get merged if they overlap in the input.
  This leads to smaller files and should put as much data as possible into the splits instead of the main module.

## 0.2.0-rc.1

- On non-wasm targets, the split function will now forward directly to the definition, so that functionality is preserved.
  On wasm targets (`target_family = "wasm"`), the cli must be run on the generated wasm object!

- Testing has been integrated by running `wasm-bindgen-test-runner`. See the `integration` directory.

## 0.2.0-rc.0

- Includes splitting of data symbols and a stronger dependency analysis
