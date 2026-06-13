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

/// Loads a split module on demand.
///
/// Backed by [`async_once_cell::OnceCell`]: a *successful* load is memoized so
/// the module is only ever loaded once, but a *failed* load leaves the cell
/// uninitialized, so a later `ensure_loaded` call retries from scratch. This is
/// what prevents a single transient load failure (flaky network, aborted
/// request) from bricking the split module for the lifetime of the session —
/// the generated JS loader additionally retries the fetch itself with backoff,
/// so reaching a hard failure here is rare. See CHANGELOG / issue #35.
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

pub async fn ensure_loaded(loader: Pin<&LazySplitLoader>) {
    // SAFETY: pin projection; `LazySplitLoader` is never moved out of.
    let loader = loader.get_ref();
    // `OnceCell` runs the init future once, serializing concurrent callers onto
    // a single in-flight attempt and waking them all when it settles. On `Err`
    // the cell stays empty, so a subsequent `ensure_loaded` starts a fresh
    // attempt rather than caching the failure.
    let result = loader
        .cell
        .get_or_try_init(SplitLoaderFuture::new(loader.load))
        .await;
    if result.is_err() {
        // The load contract is infallible from the caller's side (the generated
        // code treats split modules as must-load), so a hard failure that even
        // the JS-side fetch retries couldn't recover panics — but unlike the
        // previous implementation it is NOT cached, so reloading / a later
        // navigation can succeed.
        panic!("load callback should succeed");
    }
}

unsafe extern "C" fn load_callback(loader: *const c_void, success: bool) {
    // SAFETY: The pointer is the same as the one passed to `load`, hence comes from a call to Arc::into_raw
    unsafe { Arc::<SplitLoader>::from_raw(loader.cast()) }.complete(success);
}

/// Bridges the FFI callback into a `Future`. One instance exists per load
/// attempt and is polled by exactly one task (`OnceCell` serializes init), so a
/// single optional waker suffices — there is no need to track multiple waiters
/// here. Resolves `Ok(())` on success and `Err(())` on failure.
struct SplitLoaderFuture {
    load: LoadFn,
    shared: Option<Arc<SplitLoader>>,
}

impl SplitLoaderFuture {
    fn new(load: LoadFn) -> Self {
        Self { load, shared: None }
    }
}

impl Future for SplitLoaderFuture {
    type Output = Result<(), ()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), ()>> {
        match &self.shared {
            // First poll: kick off the load. The callback (sync or async) drives
            // `complete`. Not under any lock, so a synchronous callback is fine.
            None => {
                let shared = Arc::new(SplitLoader {
                    state: Mutex::new(SplitLoaderState::Pending(Some(cx.waker().clone()))),
                });
                // SAFETY: contract upheld by the caller of `LazySplitLoader::new`;
                // the matching `from_raw` happens in `load_callback`.
                unsafe {
                    (self.load)(
                        load_callback,
                        Arc::<SplitLoader>::into_raw(shared.clone()).cast(),
                    )
                };
                // A synchronous callback may already have marked it Done.
                let done = shared.state.lock().expect("lock poisoned").done_value();
                self.shared = Some(shared);
                match done {
                    Some(success) => Poll::Ready(ok_or_err(success)),
                    None => Poll::Pending,
                }
            }
            Some(shared) => {
                let mut state = shared.state.lock().expect("lock poisoned");
                match &mut *state {
                    SplitLoaderState::Done(success) => Poll::Ready(ok_or_err(*success)),
                    SplitLoaderState::Pending(waker) => {
                        *waker = Some(cx.waker().clone());
                        Poll::Pending
                    }
                }
            }
        }
    }
}

fn ok_or_err(success: bool) -> Result<(), ()> {
    if success {
        Ok(())
    } else {
        Err(())
    }
}

struct SplitLoader {
    state: Mutex<SplitLoaderState>,
}

impl SplitLoader {
    fn complete(&self, value: bool) {
        let waker = {
            let mut state = self.state.lock().expect("lock poisoned");
            match std::mem::replace(&mut *state, SplitLoaderState::Done(value)) {
                SplitLoaderState::Pending(waker) => waker,
                // Calling back twice for one invocation violates the documented
                // load contract (and would already have been a double `from_raw`
                // in `load_callback`); nothing useful to do here.
                SplitLoaderState::Done(_) => None,
            }
        };
        if let Some(waker) = waker {
            waker.wake();
        }
    }
}

enum SplitLoaderState {
    Pending(Option<Waker>),
    Done(bool),
}

impl SplitLoaderState {
    fn done_value(&self) -> Option<bool> {
        match self {
            SplitLoaderState::Done(v) => Some(*v),
            SplitLoaderState::Pending(_) => None,
        }
    }
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
        block_on(ensure_loaded(Pin::new(&loader)));
        block_on(ensure_loaded(Pin::new(&loader)));
        // Memoized: the second call does not re-invoke the load fn.
        assert_eq!(CALLS.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn failed_load_is_not_cached_and_a_later_call_retries() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(2); // first attempt fails, second succeeds
        let loader = unsafe { LazySplitLoader::new(scripted_load) };

        let first = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            block_on(ensure_loaded(Pin::new(&loader)));
        }));
        assert!(first.is_err(), "a failed load should panic the call");
        assert_eq!(CALLS.load(Ordering::SeqCst), 1);

        // The failure must NOT be cached: a later call starts fresh and succeeds.
        block_on(ensure_loaded(Pin::new(&loader)));
        assert_eq!(CALLS.load(Ordering::SeqCst), 2);

        // And once loaded it stays memoized.
        block_on(ensure_loaded(Pin::new(&loader)));
        assert_eq!(CALLS.load(Ordering::SeqCst), 2);
    }
}
