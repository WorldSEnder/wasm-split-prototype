use std::{
    future::Future,
    pin::Pin,
    sync::atomic::{AtomicU32, Ordering},
};
use wasm_split_helpers::{wasm_split, SplitLoaderError};

#[wasm_split(split)]
fn lazy() -> u32 {
    42
}

fn run_computation(a: u32, b: u32) -> u32 {
    // this function is longer than necessary to allow more debugger targets :)
    dbg!(a, b);
    let c = a + b;
    dbg!(c);
    c
}

#[wasm_split(split)]
pub fn args_test((a, b): (u32, u32), _: &str) -> u32 {
    run_computation(a, b)
}

// This function is also called from the webpack integration
// hence publically exposed
#[wasm_bindgen::prelude::wasm_bindgen]
pub async fn call_args_test(a: u32, b: u32) -> u32 {
    args_test((a, b), "foobar").await
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

// `fallible` keeps the user's signature: the function returns its own
// `Result<_, E>` and the macro converts a load failure into `E` via `?`/`From`.
#[wasm_split(fallible_split, fallible)]
fn fallible_lazy() -> Result<u32, SplitLoaderError> {
    Ok(42)
}

#[wasm_split(fallible_preload_split, preload(preload_fallible), fallible)]
fn fallible_preloadable() -> Result<u32, SplitLoaderError> {
    Ok(42)
}

// A use-site can declare its own error type as long as it is
// `From<SplitLoaderError>`; the load failure folds straight into it.
// On non-wasm the load can't fail, so `From::from` (and thus the only
// construction site) is unreachable there -- allow the host-only dead code.
#[derive(Debug, PartialEq)]
#[allow(dead_code)]
struct DemoError;

impl From<SplitLoaderError> for DemoError {
    fn from(_: SplitLoaderError) -> Self {
        DemoError
    }
}

#[wasm_split(custom_err_split, fallible)]
fn custom_err_lazy() -> Result<u32, DemoError> {
    Ok(42)
}

// `fallible` composed with `return_wrapper` (an async wrapper) -- the exact shape
// leptos generates for an async lazy-route view. The awaited output must itself
// be the `Result`, and `?` folds the load failure into it.
#[wasm_split(
    fallible_async_split,
    fallible,
    return_wrapper(let future = _ ; { future.await } -> Result<u32, SplitLoaderError>)
)]
fn fallible_async() -> Pin<Box<dyn Future<Output = Result<u32, SplitLoaderError>>>> {
    Box::pin(async move { Ok(42) })
}

pub static SHARED_MUT: AtomicU32 = AtomicU32::new(0xdead);

#[wasm_split(shared_mut)]
fn read_shared_mut() -> bool {
    // The test will first write a value, then load this module to execute it.
    SHARED_MUT
        .compare_exchange(0xbeaf, 42, Ordering::SeqCst, Ordering::SeqCst)
        .is_ok()
}

// Two pairs of fallible splits sharing code that main never calls: each split
// loads its pair's shared chunk alongside its own module, the shape of an
// application's lazily loaded screens. Each pair serves one test, so no other
// test has loaded its chunk before. Only the browser tests call the pairs, so
// host builds see their shared code as dead.
#[inline(never)]
#[allow(dead_code)]
fn shared_by_the_refused_pair(seed: u32) -> u32 {
    seed.rotate_left(7) ^ 0x5eed
}

#[wasm_split(refused_module, fallible)]
fn refused_module() -> Result<u32, SplitLoaderError> {
    Ok(shared_by_the_refused_pair(1))
}

#[wasm_split(refused_module_sibling, fallible)]
fn refused_module_sibling() -> Result<u32, SplitLoaderError> {
    Ok(shared_by_the_refused_pair(2))
}

#[inline(never)]
#[allow(dead_code)]
fn shared_by_the_chunk_pair(seed: u32) -> u32 {
    seed.rotate_right(5) ^ 0xc0de
}

#[wasm_split(refused_chunk_member, fallible)]
fn refused_chunk_member() -> Result<u32, SplitLoaderError> {
    Ok(shared_by_the_chunk_pair(1))
}

#[wasm_split(refused_chunk_sibling, fallible)]
fn refused_chunk_sibling() -> Result<u32, SplitLoaderError> {
    Ok(shared_by_the_chunk_pair(2))
}

/// Browser hooks for the failed-fetch tests: refuse or hold back the fetches
/// the split loaders make, count them, and count the rejections nobody
/// observed.
#[cfg(all(test, target_family = "wasm"))]
mod fetch_hook {
    use wasm_bindgen::prelude::*;
    use wasm_bindgen_futures::js_sys::Promise;

    #[wasm_bindgen(inline_js = r#"
        const original = globalThis.fetch;
        // Marks the rejections `unhandled_rejections` makes to count the others.
        const trigger = Symbol();
        let unhandled = 0;
        let refusals = 0;
        let delays = 0;
        let releaseDelayed;
        globalThis.addEventListener("unhandledrejection", (event) => {
            const report = event.reason?.[trigger];
            if (report) {
                event.preventDefault();
                report(unhandled);
            } else {
                unhandled += 1;
            }
        });

        // Fetches whose URL matches `refused` fail at once; those matching
        // `delayed` wait for `release_delayed`. An empty pattern matches nothing.
        export function hook_fetch(refused, delayed) {
            unhandled = refusals = delays = 0;
            const held = new Promise((resolve) => { releaseDelayed = resolve; });
            const refuses = refused && new RegExp(refused);
            const holds = delayed && new RegExp(delayed);
            globalThis.fetch = (input, init) => {
                const url = input instanceof Request ? input.url : String(input);
                if (refuses && refuses.test(url)) {
                    refusals += 1;
                    return Promise.reject(new TypeError(`refused by the test: ${url}`));
                }
                if (holds && holds.test(url)) {
                    delays += 1;
                    return held.then(() => original(input, init));
                }
                return original(input, init);
            };
        }
        export function release_delayed() { releaseDelayed(); }
        export function restore_fetch() { globalThis.fetch = original; }
        // Resolves to the number of rejections left unhandled so far. The browser
        // reports a rejection as unhandled in a later task, once the microtasks
        // that could still handle it have run, and reports rejections in the
        // order they happened. So this rejects a promise of its own and resolves
        // once that one is reported, after every earlier rejection.
        export function unhandled_rejections() {
            return new Promise((resolve) => Promise.reject({ [trigger]: resolve }));
        }
        export function refused_requests() { return refusals; }
        export function delayed_requests() { return delays; }
    "#)]
    extern "C" {
        pub fn hook_fetch(refused: &str, delayed: &str);
        pub fn release_delayed();
        pub fn restore_fetch();
        pub fn unhandled_rejections() -> Promise<u32>;
        pub fn refused_requests() -> u32;
        pub fn delayed_requests() -> u32;
    }
}

#[cfg(test)]
mod tests {
    #[cfg(not(target_family = "wasm"))]
    use tokio::test;
    #[cfg(target_family = "wasm")]
    use wasm_bindgen_test::wasm_bindgen_test as test;

    /// Shim for std::assert_matches! which got stabilized in 1.96
    macro_rules! assert_matches {
        ($scrut:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
            match $scrut {
                $( $pattern )|+ $( if $guard )? => {}
                ref scrutinee => panic!(
                    "\
assertion `scrutinee matches pattern` failed
 scrutinee: {scrutinee:?}
   pattern: {}",
                    std::stringify!($($pattern)|+ $(if $guard)?)
                ),
            }
        };
        ($scrut:expr, $(|)? $( $pattern:pat_param )|+ $( if $guard: expr )? , $($arg:tt)+) => {
            match $scrut {
                $( $pattern )|+ $( if $guard )? => {}
                ref scrutinee => panic!(
                    "\
assertion `scrutinee matches pattern` failed: {}
 scrutinee: {scrutinee:?}
   pattern: {}",
                    std::format_args!( $($arg)+ ),
                    std::stringify!($($pattern)|+ $(if $guard)?)
                ),
            }
        };
    }

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
        // A non-`fallible` preload `expects` the load to succeed.
        let () = crate::preload_it().await;
        assert_eq!(
            crate::preloadable().await,
            42,
            "should execute the preloadable function correctly"
        );
    }

    #[test]
    pub async fn it_supports_fallible_wrappers() {
        assert_matches!(
            crate::fallible_lazy().await,
            Ok(42),
            "a fallible wrapper returns Ok(_) on a successful load"
        );
    }

    #[test]
    pub async fn it_supports_fallible_preload() {
        // With `fallible`, the preload returns `Result<(), SplitLoaderError>`.
        crate::preload_fallible()
            .await
            .expect("preload should succeed");
        assert_matches!(crate::fallible_preloadable().await, Ok(42));
    }

    #[test]
    pub async fn it_supports_fallible_custom_error() {
        // The wrapper keeps the user's signature; a load failure would convert
        // into `DemoError` via `From<SplitLoaderError>`. On success it is `Ok`.
        assert_matches!(crate::custom_err_lazy().await, Ok(42));
    }

    #[test]
    pub async fn it_supports_fallible_with_return_wrapper() {
        // `fallible` + `return_wrapper` (the leptos async-view shape).
        assert_matches!(crate::fallible_async().await, Ok(42));
    }

    #[cfg(target_family = "wasm")]
    #[test]
    pub async fn it_observes_a_module_request_failing_while_its_chunk_loads() {
        use crate::fetch_hook::*;
        // The module's own fetch fails at once while its shared chunk is held
        // back. The chunk is released only once that failure, if unobserved,
        // has been reported: a spawned task first runs after the load below
        // has started its requests.
        hook_fetch(r"/split_refused_module\.wasm$", r"/chunk_\d+\.wasm$");
        wasm_bindgen_futures::spawn_local(async {
            let _ = unhandled_rejections().await;
            release_delayed();
        });
        let refused = crate::refused_module().await;
        let unhandled = unhandled_rejections().await.unwrap();
        let held_back = delayed_requests();
        restore_fetch();
        assert!(
            held_back > 0,
            "no chunk request was held back: the split no longer loads a chunk"
        );
        assert_matches!(refused, Err(_), "a refused module fetch fails the load");
        assert_eq!(
            unhandled, 0,
            "the refused fetch escaped as an unhandled rejection"
        );
        assert_matches!(
            crate::refused_module().await,
            Ok(_),
            "the next attempt loads"
        );
        assert_matches!(crate::refused_module_sibling().await, Ok(_));
    }

    #[cfg(target_family = "wasm")]
    #[test]
    pub async fn it_observes_a_module_request_abandoned_after_a_chunk_fails() {
        use crate::fetch_hook::*;
        // The chunk fails first, so the loader gives up before it ever awaits
        // the module's own fetch, which fails as well.
        hook_fetch(r"/(chunk_\d+|split_refused_chunk_member)\.wasm$", "");
        let refused = crate::refused_chunk_member().await;
        let unhandled = unhandled_rejections().await.unwrap();
        let refusals = refused_requests();
        restore_fetch();
        assert!(
            refusals >= 2,
            "expected the module and a chunk to be refused, got {refusals} refusals: \
             the split no longer loads a chunk"
        );
        assert_matches!(refused, Err(_), "a refused chunk fails the load");
        assert_eq!(
            unhandled, 0,
            "the refused fetch escaped as an unhandled rejection"
        );
        assert_matches!(
            crate::refused_chunk_member().await,
            Ok(_),
            "the next attempt loads"
        );
        assert_matches!(crate::refused_chunk_sibling().await, Ok(_));
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
