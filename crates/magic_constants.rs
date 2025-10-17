//! This file contains constants that are used by multiple crates.
//! Since publishing a really tiny crate for this makes no sense, and would only lead
//! to versioning problems and more, this is just a tiny source file that is included
//! by sym-linking it in the relevant crates.
#![allow(unused)]

// Note: due to parsing behaviour, all tags should fit in a u7
#[repr(u8)]
pub enum SubsectionTag {
    Version = 1,
}
// This is a macro instead of a constant so that it can be used in #[link_section]
macro_rules! __link_section {
    () => {
        "__wasm_split_unstable"
    };
}
pub(crate) use __link_section as link_section;
pub const LINK_SECTION: &str = link_section!();

#[repr(u8)] // Future note: encode as uleb.
pub enum Version {
    Version1 = 1,
}
