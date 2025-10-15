use wasm_split_helpers::wasm_split;

static SHARED_ARG: &str = include_str!("./long_embedded_text.txt");

#[inline(never)]
fn shared(arg: &str) -> usize {
    arg.chars().filter(|c| c.is_ascii_uppercase()).count()
}

macro_rules! define_share {
    ($( $module:ident )*) => {
        $( #[wasm_split($module)]
        #[allow(unused)]
        fn $module() -> usize {
            crate::shared(SHARED_ARG)
        } )*
    };
}

define_share!(a b c d e f g h i j k l m n o p q r s t u v w x y z);

#[cfg(test)]
mod tests {
    #[cfg(not(target_family = "wasm"))]
    use tokio::test;
    #[cfg(target_family = "wasm")]
    use wasm_bindgen_test::wasm_bindgen_test as test;

    macro_rules! call_all {
        ( $($module:ident)*) => {
            vec![ $( crate::$module().await ),* ]
        }
    }

    #[test]
    pub async fn all_same_result() {
        let results = call_all!(a b c d e f g h i j k l m n o p q r s t u v w x y z);
        assert!(
            results.windows(2).all(|w| w[0] == w[1]),
            "expected all answers to be the same"
        );
    }
}
