//! This test case tests support for spliting a module that Closure::wrap
//!
//! Closure::wrap is special because it works with a special method in wasm-bindgen, `__wbindgen_describe_closure`,
//! which is getting rewritten in a different way than imports to/exports from wasm.
//! For more details, see the bug report in https://github.com/leptos-rs/leptos/issues/4347#issuecomment-3378800158,
//! and specifically https://github.com/leptos-rs/leptos/issues/4347#issuecomment-3383869144 for a quick run down.
use wasm_bindgen::JsValue;
use wasm_bindgen::prelude::{Closure, wasm_bindgen};
use wasm_split_helpers::wasm_split;

#[wasm_bindgen(inline_js = "export function call_closure(f, a) { return f(a) }")]
extern "C" {
    fn call_closure(fun: &JsValue, arg: u32) -> u32;
}

#[wasm_split(split)]
fn in_split() -> u32 {
    let closure = Closure::wrap(Box::new(move |e: u32| e) as Box<dyn FnMut(_) -> _>);
    let closure = std::hint::black_box(closure); // make sure the compiler doesn't optimize the instantiation out
    call_closure(closure.as_ref(), 42)
}

#[cfg(test)]
mod tests {
    use wasm_bindgen_test::wasm_bindgen_test as test;

    #[test]
    pub async fn it_can_call_the_closure() {
        assert_eq!(
            crate::in_split().await,
            42,
            "should have successfully loaded and executed"
        );
    }
}
