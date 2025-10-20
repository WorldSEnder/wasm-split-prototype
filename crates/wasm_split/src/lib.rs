//! Annotate functions as appropriate split points for lazy-loaded code in WebAssembly (WASM).
//!
//! ```no_run
//! use wasm_split_helpers::wasm_split;
//!
//! #[wasm_split(rot13_cipher)]
//! fn rot13(text: &str) -> String {
//!     text.chars()
//!         .map(|c| match c {
//!             'A'..='M' | 'a'..='m' => ((c as u8) + 13) as char,
//!             'N'..='Z' | 'n'..='z' => ((c as u8) - 13) as char,
//!             _ => c,
//!         })
//!         .collect()
//! }
//!
//! async fn check_passphrase(cipher: &str) {
//!     assert_eq!(rot13(cipher).await, "furrfu", "wrong passphrase!");
//! }
//! # let _ = check_passphrase;
//! ```

pub use wasm_split_macros::wasm_split;

#[doc(hidden)]
pub mod rt;

mod marker;
