use wasm_split::wasm_split;

#[wasm_split(split)]
fn lazy() -> u32 {
    42
}

#[cfg(test)]
mod tests {
    use wasm_bindgen_test::wasm_bindgen_test as test;

    #[test]
    pub async fn it_runs() {
        assert_eq!(
            crate::lazy().await,
            42,
            "should have successfully loaded and executed"
        );
    }
}
