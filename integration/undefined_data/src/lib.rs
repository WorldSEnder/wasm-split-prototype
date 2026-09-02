//! The main module references a data symbol that nothing defines. Linked
//! under `--allow-undefined`, wasm-ld keeps the symbol in the symbol table
//! as undefined, resolves every reference to address 0, and leaves a
//! `R_WASM_MEMORY_ADDR_*` relocation against it. The splitter must accept
//! such a module: an undefined data symbol has no definition to place or
//! relocate, so its references keep the linker's value.
//!
//! rustc incremental builds produce this shape in the wild when a reused
//! object still references a promoted anonymous global whose name changed
//! (rust-lang/rust#81280).
//!
//! Wasm only: a native linker rejects the undefined reference outright.
#![cfg(target_family = "wasm")]

use wasm_split_helpers::wasm_split;

extern "C" {
    static UNDEFINED_DATA: u8;
}

/// The address the linker resolved for the undefined data symbol.
pub fn undefined_data_address() -> usize {
    // Taking the address, not reading through it, keeps the relocation
    // against the undefined symbol without touching address 0.
    unsafe { core::ptr::addr_of!(UNDEFINED_DATA) as usize }
}

#[wasm_split(split)]
fn lazy() -> u32 {
    42
}

#[cfg(test)]
mod tests {
    use wasm_bindgen_test::wasm_bindgen_test as test;

    // This pulls in the magic marker which we assert in all tests
    const _: () = {
        let _ = wasm_split_helpers::rt::ensure_loaded;
    };

    #[test]
    fn undefined_data_keeps_the_linker_address() {
        assert_eq!(
            super::undefined_data_address(),
            0,
            "an undefined data symbol resolves to address 0 and must not be relocated"
        );
    }

    #[test]
    async fn split_still_loads() {
        assert_eq!(super::lazy().await, 42);
    }
}
