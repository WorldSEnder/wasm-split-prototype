use std::{
    ffi::c_void,
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll, Waker},
};

pub use wasm_split_macros::wasm_split;

pub type LoadCallbackFn = unsafe extern "C" fn(*const c_void, bool) -> ();
pub type LoadFn = unsafe extern "C" fn(LoadCallbackFn, *const c_void) -> ();

type Lazy = async_once_cell::Lazy<Option<()>, SplitLoaderFuture>;

pub struct LazySplitLoader {
    lazy: Lazy,
}

impl LazySplitLoader {
    /// # Safety
    /// The load function must be safely callable with a callback-fn-pointer and data.
    /// It must call the callback function exactly once with the given data and a success state.
    pub const unsafe fn new(load: LoadFn) -> Self {
        Self {
            lazy: Lazy::new(SplitLoaderFuture::new(load)),
        }
    }
}

pub async fn ensure_loaded(loader: Pin<&LazySplitLoader>) {
    // SAFETY: this is just pin projection
    let inner = unsafe { Pin::new_unchecked(&loader.lazy) };
    inner.await.expect("load callback should succeed");
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
    type Output = Option<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<()>> {
        match self.state {
            SplitLoaderFutState::Deferred(load) => {
                let shared_state = Arc::new(SplitLoader {
                    state: Mutex::new(SplitLoaderState::Waiting(cx.waker().clone())),
                });
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
                SplitLoaderState::Done(value) => Poll::Ready(value.then_some(())),
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

unsafe extern "C" fn load_callback(loader: *const c_void, success: bool) {
    // SAFETY: The pointer is the same as the one passed to `load`, hence comes from a call to Arc::into_raw
    unsafe { Arc::<SplitLoader>::from_raw(loader.cast()) }.complete(success);
}

struct SplitLoader {
    state: Mutex<SplitLoaderState>,
}

impl SplitLoader {
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
