use std::pin::Pin;
use wasm_split_helpers::wasm_split;

#[wasm_split(split)]
fn lazy() -> u32 {
    42
}

#[wasm_split(split)]
fn args_test((a, b): (u32, u32), _: &str) -> u32 {
    a + b
}

#[wasm_split(
    split,
    return_wrapper(let future = _ ; { future.await } -> u32)
)]
fn async_fn() -> Pin<Box<dyn Future<Output = u32>>> {
    Box::pin(async move { 42 })
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

    #[test]
    pub async fn it_pattern_matches() {
        assert_eq!(
            crate::args_test((20, 10), "ignored").await,
            30,
            "should pattern match and sum arguments"
        );
    }
}
