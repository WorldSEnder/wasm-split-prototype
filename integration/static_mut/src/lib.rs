//! This test case is a bit brittle as we try to force a specific case in the analysis,
//! specifically we target the branch when we detect an "overlong segment" and force data to
//! be put into the main module.
//! We then have a static item (PUB_TEST_FN) that references a dead function and which
//! itself is unreachable. This will have to be relocated but is not reachable from any
//! of the output functions.

fn reachable_from_env() -> u32 {
    42
}

#[no_mangle]
pub static mut PUB_TEST_FN: fn() -> u32 = reachable_from_env;

#[cfg(test)]
mod tests {
    use wasm_bindgen::prelude::{wasm_bindgen, JsValue};
    use wasm_bindgen_test::wasm_bindgen_test as test;

    #[wasm_bindgen(inline_js = r#"
        export function call_static_mut(inst) {
            var exports = inst.exports;
            var mem = new DataView(exports.memory.buffer);
            var fn_idx = mem.getInt32(exports.PUB_TEST_FN, true);
            var f = exports.__indirect_function_table.get(fn_idx);
            return f();
        }
    "#)]
    extern "C" {
        fn call_static_mut(fun: &JsValue) -> u32;
    }

    // This pulls in the magic marker which we assert in all tests
    const _: () = {
        let _ = wasm_split_helpers::rt::ensure_loaded;
    };

    #[test]
    fn call_through_abi() {
        let result = call_static_mut(&wasm_bindgen::instance());
        assert_eq!(
            result, 42,
            "can call symbol exposed through no_mangle static mut"
        );
    }
}
