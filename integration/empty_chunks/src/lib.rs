//! `zero_a` and `zero_b` share only `NO_DATA`. The splitter never relocates a zero-sized
//! symbol, so the chunk shared by the pair defines no function and no data, and no module
//! must be written for it.
//!
//! The split bodies differ so that optimized builds don't fold them into one function.

use wasm_split_helpers::wasm_split;

static NO_DATA: [u8; 0] = [];

#[wasm_split(zero_a)]
pub fn zero_a() -> usize {
    NO_DATA.as_ptr() as usize
}

#[wasm_split(zero_b)]
pub fn zero_b() -> usize {
    NO_DATA.as_ptr() as usize + 1
}

#[cfg(test)]
mod tests {
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    async fn zero_sized_static_has_one_address() {
        assert_eq!(crate::zero_a().await + 1, crate::zero_b().await);
    }
}
