use std::{future::Future, pin::Pin};

use wasm_split_helpers::wasm_split;

#[wasm_split(cyclic)]
fn requires_foo() -> Pin<Box<dyn Future<Output = ()>>> {
    Box::pin(async move {
        let () = requires_bar().await.await;
    })
}

#[wasm_split(cyclic)]
fn requires_bar() -> Pin<Box<dyn Future<Output = ()>>> {
    Box::pin(async move {
        let () = requires_foo().await.await;
    })
}

#[cfg(test)]
mod tests {}
