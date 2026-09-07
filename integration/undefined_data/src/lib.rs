//! The main module references a data symbol with weak linkage. This will
//! resolves every reference to address 0, but will still leaves a
//! `R_WASM_MEMORY_ADDR_*` relocation against it. The splitter must accept
//! such a module.
//!
//! This is to mock a bug (rust-lang/rust#81280) occuring in the wild. In
//! this case, rustc incremental builds produce a similar shape. Here, an
//! undefined reference to a promoted anonymous global whose name changed
//! gets resolves to address 0 with a linker arg, `--allow-undefined`,
//! passed to wasm-ld as a workaround.
#![cfg_attr(has_weak_linkage_needs_feature, feature(linkage))]

use wasm_split_helpers::wasm_split;

#[cfg(has_weak_linkage)]
pub mod weak {
    unsafe extern "C" {
        #[linkage = "extern_weak"]
        safe static UNDEFINED_DATA: Option<&'static i32>;
    }

    /// The address the linker resolved for the undefined data symbol.
    pub fn undefined_data_address() -> usize {
        // Taking the address, not reading through it, keeps the relocation
        // against the undefined symbol without touching address 0.
        UNDEFINED_DATA
            .map(|data| (data as *const i32).addr())
            .unwrap_or(0)
    }
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

    #[cfg(has_weak_linkage)]
    #[test]
    fn undefined_data_keeps_the_linker_address() {
        assert_eq!(
            super::weak::undefined_data_address(),
            0,
            "an undefined data symbol resolves to address 0 and must not be relocated"
        );
    }

    #[test]
    async fn split_still_loads() {
        assert_eq!(super::lazy().await, 42);
    }
}
