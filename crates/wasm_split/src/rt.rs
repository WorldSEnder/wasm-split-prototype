use std::{
    ffi::c_void,
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll, Waker},
};

pub type LoadCallbackFn = unsafe extern "C" fn(*const c_void, bool) -> ();
pub type LoadFn = unsafe extern "C" fn(LoadCallbackFn, *const c_void) -> ();

/// How many times a single `ensure_loaded` call re-invokes the load function
/// after a failed attempt before giving up. A failed attempt also resets the
/// loader to its unloaded state, so *later* `ensure_loaded` calls start fresh
/// attempts even after one call exhausted its budget — a transient network
/// failure no longer bricks the split module for the lifetime of the session.
const MAX_ATTEMPTS_PER_CALL: usize = 3;

pub struct LazySplitLoader {
    load: LoadFn,
    state: Mutex<LoadState>,
}

enum LoadState {
    /// No load attempt is running (initial state, and the state after a
    /// failed attempt has been observed).
    NotStarted,
    /// A load attempt is in flight; all concurrent waiters share it.
    InFlight(Arc<SplitLoader>),
    /// The module loaded successfully. Terminal.
    Ready,
}

impl LazySplitLoader {
    /// # Safety
    /// The load function must be safely callable with a callback-fn-pointer and data.
    /// It must call the callback function exactly once per invocation with the
    /// given data and a success state.
    pub const unsafe fn new(load: LoadFn) -> Self {
        Self {
            load,
            state: Mutex::new(LoadState::NotStarted),
        }
    }
}

pub async fn ensure_loaded(loader: Pin<&LazySplitLoader>) {
    let loader = loader.get_ref();
    let mut attempts = 0;
    loop {
        // Join the in-flight attempt, or start a new one.
        let attempt = {
            let mut state = loader.state.lock().expect("lock poisoned");
            match &*state {
                LoadState::Ready => return,
                LoadState::InFlight(shared) => shared.clone(),
                LoadState::NotStarted => {
                    let shared = Arc::new(SplitLoader::new());
                    // SAFETY: upheld by the caller of `LazySplitLoader::new`.
                    unsafe { (loader.load)(load_callback, Arc::into_raw(shared.clone()).cast()) };
                    *state = LoadState::InFlight(shared.clone());
                    shared
                }
            }
        };

        attempts += 1;
        let success = SplitLoaderFuture {
            shared: attempt.clone(),
        }
        .await;

        let mut state = loader.state.lock().expect("lock poisoned");
        if success {
            *state = LoadState::Ready;
            return;
        }
        // The attempt failed. Reset to NotStarted so the next iteration (or a
        // later `ensure_loaded` call) starts a fresh attempt — but only if the
        // recorded attempt is still ours; a concurrent waiter may have reset
        // and restarted already, in which case we join that attempt instead.
        if let LoadState::InFlight(current) = &*state {
            if Arc::ptr_eq(current, &attempt) {
                *state = LoadState::NotStarted;
            }
        }
        drop(state);

        if attempts >= MAX_ATTEMPTS_PER_CALL {
            // Give up on *this* call. The loader state is already reset, so
            // future calls still retry.
            panic!("load callback should succeed");
        }
    }
}

unsafe extern "C" fn load_callback(loader: *const c_void, success: bool) {
    // SAFETY: The pointer is the same as the one passed to `load`, hence comes from a call to Arc::into_raw
    unsafe { Arc::<SplitLoader>::from_raw(loader.cast()) }.complete(success);
}

/// One load attempt: completed exactly once by `load_callback`, awaited by any
/// number of concurrent `ensure_loaded` callers.
struct SplitLoader {
    state: Mutex<SplitLoaderState>,
}

enum SplitLoaderState {
    Pending(Vec<Waker>),
    Done(bool),
}

impl SplitLoader {
    fn new() -> Self {
        Self {
            state: Mutex::new(SplitLoaderState::Pending(Vec::new())),
        }
    }

    fn complete(&self, value: bool) {
        let wakers = {
            let mut state = self.state.lock().expect("lock poisoned");
            match std::mem::replace(&mut *state, SplitLoaderState::Done(value)) {
                SplitLoaderState::Pending(wakers) => wakers,
                // The load function's contract is one callback per invocation;
                // tolerate a duplicate rather than corrupting state.
                SplitLoaderState::Done(_) => Vec::new(),
            }
        };
        for waker in wakers {
            waker.wake();
        }
    }
}

struct SplitLoaderFuture {
    shared: Arc<SplitLoader>,
}

impl Future for SplitLoaderFuture {
    type Output = bool;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<bool> {
        let mut state = self.shared.state.lock().expect("lock poisoned");
        match &mut *state {
            SplitLoaderState::Done(value) => Poll::Ready(*value),
            SplitLoaderState::Pending(wakers) => {
                if !wakers.iter().any(|w| w.will_wake(cx.waker())) {
                    wakers.push(cx.waker().clone());
                }
                Poll::Pending
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::task::{RawWaker, RawWakerVTable};

    // Drive a future to completion with a no-op waker. The test load fns
    // complete synchronously inside `load`, so polling in a loop suffices.
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
    fn retries_within_one_call_until_success() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(3); // fail, fail, succeed
        let loader = unsafe { LazySplitLoader::new(scripted_load) };
        block_on(ensure_loaded(Pin::new(&loader)));
        assert_eq!(CALLS.load(Ordering::SeqCst), 3);
        // Once Ready, no further load invocations.
        block_on(ensure_loaded(Pin::new(&loader)));
        assert_eq!(CALLS.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn later_call_retries_after_exhausted_call() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(MAX_ATTEMPTS_PER_CALL + 1); // first call exhausts its budget
        let loader = unsafe { LazySplitLoader::new(scripted_load) };
        let exhausted = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            block_on(ensure_loaded(Pin::new(&loader)));
        }));
        assert!(exhausted.is_err(), "first call should give up");
        assert_eq!(CALLS.load(Ordering::SeqCst), MAX_ATTEMPTS_PER_CALL);
        // The failure must NOT be cached: a later call starts fresh and succeeds.
        block_on(ensure_loaded(Pin::new(&loader)));
        assert_eq!(CALLS.load(Ordering::SeqCst), MAX_ATTEMPTS_PER_CALL + 1);
    }

    #[test]
    fn immediate_success_loads_once() {
        let _guard = TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        reset_script(1);
        let loader = unsafe { LazySplitLoader::new(scripted_load) };
        block_on(ensure_loaded(Pin::new(&loader)));
        block_on(ensure_loaded(Pin::new(&loader)));
        assert_eq!(CALLS.load(Ordering::SeqCst), 1);
    }
}
