use std::{
    future::Future,
    pin::Pin,
    sync::atomic::{AtomicU32, Ordering},
};
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

mod smoke {
    use ::wasm_split_helpers as wsplit_alias;
    mod wasm_split_helpers {}

    #[wsplit_alias::wasm_split(
        split,
        wasm_split_path = wsplit_alias
    )]
    pub fn uses_crate_reexport() -> u32 {
        42
    }
}

#[wasm_split(preloadable_split, preload(preload_it))]
fn preloadable() -> u32 {
    42
}

pub static SHARED_MUT: AtomicU32 = AtomicU32::new(0xdead);

#[wasm_split(shared_mut)]
fn read_shared_mut() -> bool {
    // The test will first write a value, then load this module to execute it.
    SHARED_MUT
        .compare_exchange(0xbeaf, 42, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
}

#[cfg(test)]
mod tests {
    #[cfg(not(target_family = "wasm"))]
    use tokio::test;
    #[cfg(target_family = "wasm")]
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

    #[test]
    pub async fn it_runs_async_fns() {
        assert_eq!(
            crate::async_fn().await,
            42,
            "should load and await the future"
        );
    }

    #[test]
    pub async fn it_can_handle_wasm_split_path() {
        assert_eq!(
            crate::smoke::uses_crate_reexport().await,
            42,
            "should refer to the crate by the alias"
        );
    }

    #[test]
    pub async fn it_can_preload_a_split() {
        crate::preload_it().await;
        assert_eq!(
            crate::preloadable().await,
            42,
            "should execute the preloadable function correctly"
        );
    }

    #[test]
    pub async fn it_supports_atomic_statics() {
        crate::SHARED_MUT.store(0xbeaf, super::Ordering::SeqCst);
        assert!(
            crate::read_shared_mut().await,
            "Didn't read the value we stored"
        );
        assert_eq!(
            crate::SHARED_MUT.load(super::Ordering::SeqCst),
            42,
            "should have successfully stored its value"
        );
    }
}
