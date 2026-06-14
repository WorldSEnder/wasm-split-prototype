use std::{
    ffi::c_void,
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll, Waker},
};

use async_once_cell::OnceCell;

pub type LoadCallbackFn = unsafe extern "C" fn(*const c_void, bool) -> ();
pub type LoadFn = unsafe extern "C" fn(LoadCallbackFn, *const c_void) -> ();

/// A split-module load attempt failed.
///
/// Exact error information is currently not tracked but logged to the browser
/// console. If you need access to the details, please open an issue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SplitLoaderError(());

impl std::fmt::Display for SplitLoaderError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("failed to load split module")
    }
}

impl std::error::Error for SplitLoaderError {}

/// Loads a split module on demand, memoizing only successful loads.
///
/// A failed load leaves the loader uninitialized, so a later `ensure_loaded`
/// retries from scratch rather than caching the failure for the session.
pub struct LazySplitLoader {
    load: LoadFn,
    cell: OnceCell<()>,
}

impl LazySplitLoader {
    /// # Safety
    /// The load function must be safely callable with a callback-fn-pointer and data.
    /// It must call the callback function exactly once per invocation with the given
    /// data and a success state. (It may be invoked more than once across retries,
    /// each invocation calling back exactly once.)
    pub const unsafe fn new(load: LoadFn) -> Self {
        Self {
            load,
            cell: OnceCell::new(),
        }
    }
}

/// Ensures the split module is loaded, returning [`SplitLoaderError`] if the
/// load attempt failed.
///
/// On failure the loader is left uninitialized rather than caching the error,
/// so calling `ensure_loaded` again starts a fresh attempt. A `#[wasm_split(..)]`
/// function opts into seeing this error via the `fallible` option; otherwise the
/// generated wrapper expects a successful load.
pub async fn ensure_loaded(loader: Pin<&LazySplitLoader>) -> Result<(), SplitLoaderError> {
    // SAFETY: pin projection; `LazySplitLoader` is never moved out of.
    let loader = loader.get_ref();
    // `OnceCell` runs the init future once, serializing concurrent callers onto a
    // single in-flight attempt. On `Err` the cell stays empty, so a later call
    // starts a fresh attempt rather than caching the failure.
    loader
        .cell
        .get_or_try_init(SplitLoaderFuture::new(loader.load))
        .await
        .map(|_| ())
}

unsafe extern "C" fn load_callback(loader: *const c_void, success: bool) {
    // SAFETY: The pointer is the same as the one passed to `load`, hence comes from a call to Arc::into_raw
    unsafe { Arc::<SplitLoader>::from_raw(loader.cast()) }.complete(success);
}

enum SplitLoaderFutState {
    Deferred(LoadFn),
    Pending(Arc<SplitLoader>),
}

struct SplitLoaderFuture {
    state: SplitLoaderFutState,
}

impl SplitLoaderFuture {
    const fn new(load: LoadFn) -> Self {
        SplitLoaderFuture {
            state: SplitLoaderFutState::Deferred(load),
        }
    }
}

impl Future for SplitLoaderFuture {
    type Output = Result<(), SplitLoaderError>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), SplitLoaderError>> {
        match self.state {
            SplitLoaderFutState::Deferred(load) => {
                let shared_state = Arc::new(SplitLoader {
                    state: Mutex::new(SplitLoaderState::Waiting(cx.waker().clone())),
                });
                // SAFETY: the contract is upheld by the caller of `LazySplitLoader::new`;
                // the matching `from_raw` happens in `load_callback`. A synchronous
                // callback completes `shared_state` and wakes this waker, so the future
                // is re-polled and observes `Done` below.
                unsafe {
                    load(
                        load_callback,
                        Arc::<SplitLoader>::into_raw(shared_state.clone()).cast(),
                    )
                };
                self.state = SplitLoaderFutState::Pending(shared_state);
                Poll::Pending
            }
            SplitLoaderFutState::Pending(ref state) => state.update(|state| match state {
                SplitLoaderState::Done(value) => Poll::Ready(load_result(*value)),
                SplitLoaderState::Waiting(ref mut waker) => {
                    *waker = cx.waker().clone();
                    Poll::Pending
                }
            }),
        }
    }
}

// our future should never get canceled before completion. We could assert this?
// impl Drop for SplitLoaderFuture { }

/// Maps the success flag from the load callback into the public load result.
/// This is the single place a [`SplitLoaderError`] is constructed, ready to
/// carry richer detail in the future.
fn load_result(success: bool) -> Result<(), SplitLoaderError> {
    // Equivalent to `success.then_some(()).ok_or(SplitLoaderError(()))`, or
    // `success.ok_or(SplitLoaderError(()))` should `bool::ok_or` ever stabilize.
    if success {
        Ok(())
    } else {
        Err(SplitLoaderError(()))
    }
}

struct SplitLoader {
    state: Mutex<SplitLoaderState>,
}

impl SplitLoader {
    /// Runs `f` while holding the state lock. Keeping every caller's closure
    /// panic-free is what guarantees the lock is never poisoned, so the
    /// `expect("lock poisoned")` never fires.
    fn update<R>(&self, f: impl FnOnce(&mut SplitLoaderState) -> R) -> R {
        let mut state = self.state.lock().expect("lock poisoned");
        f(&mut state)
    }
    fn complete(&self, value: bool) {
        self.update(|state| {
            if let SplitLoaderState::Waiting(waker) =
                std::mem::replace(state, SplitLoaderState::Done(value))
            {
                waker.wake();
            }
        });
    }
}

enum SplitLoaderState {
    Waiting(Waker),
    Done(bool),
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::task::{RawWaker, RawWakerVTable};

    fn block_on<F: Future>(fut: F) -> F::Output {
        fn noop_raw_waker() -> RawWaker {
            fn no_op(_: *const ()) {}
            fn clone(_: *const ()) -> RawWaker {
                noop_raw_waker()
            }
            static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, no_op, no_op, no_op);
            RawWaker::new(std::ptr::null(), &VTABLE)
        }
        let waker = unsafe { Waker::from_raw(noop_raw_waker()) };
        let mut cx = Context::from_waker(&waker);
        let mut fut = std::pin::pin!(fut);
        for _ in 0..1000 {
            if let Poll::Ready(out) = fut.as_mut().poll(&mut cx) {
                return out;
            }
        }
        panic!("future did not complete");
    }

    // The scripted load fn shares these statics, so tests must not interleave.
    static TEST_LOCK: Mutex<()> = Mutex::new(());
    static CALLS: AtomicUsize = AtomicUsize::new(0);
    static SUCCEED_FROM_CALL: AtomicUsize = AtomicUsize::new(0);

    unsafe extern "C" fn scripted_load(cb: LoadCallbackFn, data: *const c_void) {
        let call = CALLS.fetch_add(1, Ordering::SeqCst) + 1;
        let success = call >= SUCCEED_FROM_CALL.load(Ordering::SeqCst);
        unsafe { cb(data, success) };
    }

    fn reset_script(succeed_from_call: usize) {
        CALLS.store(0, Ordering::SeqCst);
        SUCCEED_FROM_CALL.store(succeed_from_call, Ordering::SeqCst);
    }

    #[test]
    fn immediate_success_loads_once() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(1);
        let loader = unsafe { LazySplitLoader::new(scripted_load) };
        assert!(block_on(ensure_loaded(Pin::new(&loader))).is_ok());
        assert!(block_on(ensure_loaded(Pin::new(&loader))).is_ok());
        // Memoized: the second call does not re-invoke the load fn.
        assert_eq!(CALLS.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn failed_load_is_not_cached_and_a_later_call_retries() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(2); // first attempt fails, second succeeds
        let loader = unsafe { LazySplitLoader::new(scripted_load) };

        // A failed load surfaces as `Err` rather than panicking.
        assert_eq!(
            block_on(ensure_loaded(Pin::new(&loader))),
            Err(SplitLoaderError(()))
        );
        assert_eq!(CALLS.load(Ordering::SeqCst), 1);

        // The failure must NOT be cached: a later call starts fresh and succeeds.
        assert!(block_on(ensure_loaded(Pin::new(&loader))).is_ok());
        assert_eq!(CALLS.load(Ordering::SeqCst), 2);

        // And once loaded it stays memoized.
        assert!(block_on(ensure_loaded(Pin::new(&loader))).is_ok());
        assert_eq!(CALLS.load(Ordering::SeqCst), 2);
    }

    // Mirrors the glue that `#[wasm_split(.., fallible)]` generates for a
    // wrapper: `preload().await?; Ok({ <body> })` (where `preload` is a thin
    // wrapper over `ensure_loaded`). The browser test harness can't inject a
    // real load failure, so this asserts the macro-emitted shape directly: a
    // failed load short-circuits to `Err` *without* running the body, and a
    // later call recovers because the failure was not cached.
    #[test]
    fn fallible_wrapper_shape_short_circuits_then_recovers() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(2); // first attempt fails, second succeeds
        let loader = unsafe { LazySplitLoader::new(scripted_load) };

        async fn fallible_wrapper(
            loader: Pin<&LazySplitLoader>,
            body: impl FnOnce() -> u32,
        ) -> Result<u32, SplitLoaderError> {
            ensure_loaded(loader).await?;
            Ok(body())
        }

        let body_runs = AtomicUsize::new(0);

        // Load fails -> `Err`, and the wrapped body never runs.
        let first = block_on(fallible_wrapper(Pin::new(&loader), || {
            body_runs.fetch_add(1, Ordering::SeqCst);
            42
        }));
        assert_eq!(first, Err(SplitLoaderError(())));
        assert_eq!(
            body_runs.load(Ordering::SeqCst),
            0,
            "the function body must not run when the load fails"
        );

        // Caller retries: the failure was not cached, so the load now succeeds,
        // the body runs, and the wrapper yields `Ok`.
        let second = block_on(fallible_wrapper(Pin::new(&loader), || {
            body_runs.fetch_add(1, Ordering::SeqCst);
            42
        }));
        assert_eq!(second, Ok(42));
        assert_eq!(body_runs.load(Ordering::SeqCst), 1);
    }
}
