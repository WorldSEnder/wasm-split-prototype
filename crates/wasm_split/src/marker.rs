#![cfg(target_family = "wasm")]

#[path = "./magic_constants.rs"]
mod magic_constants;
use magic_constants::{link_section, SubsectionTag, Version};

const fn usize_to_u32(len: usize) -> u32 {
    // We ... don't assume usize is bigger or smaller than a u32,
    // which complicates this a bit.
    let len: u128 = len as u128;
    if len >= u32::MAX as u128 {
        panic!("usize too large to fit into u32");
    }
    len as u32
}
const fn u32_uleb_len(num: u32) -> usize {
    // for simplicity, we encode either 1 or 5 bytes.
    match num {
        ..0x80 => 1,
        _ => 5,
    }
}

const fn u32_uleb(num: u32, buffer: &mut [u8]) {
    debug_assert!(buffer.len() == u32_uleb_len(num));
    let mut rem = num;
    let mut continue_mask: u8 = 0x00;
    const SHIFT: usize = 7;

    let mut i = buffer.len();
    while i > 0 {
        i -= 1;
        let b = &mut buffer[i];

        let low = (rem & 0x7f) as u8;
        rem >>= SHIFT;
        *b = continue_mask | low;
        continue_mask = 0x80;
    }
}

// We use a wasm-typical approach of a self-describing subsection.
// The linker will, in arbitrary order, concat the bytes marked by
// #[link_section] below into one custom section. Each byte-slice
// will encode a tag describing its content encoding and its own
// length.
//
// This format follows the [`Subsections`] reader in `wasmparser`:
// ```
// | tag: u8 | len: u32 as uleb | payload: [u8; len] |
// ```
macro_rules! encode_subsection {
    ( const ($LEN:ident, fn $encoder:ident) = ($tag:expr, $payload_len:expr, $encode_payload:path) ; ) => {
        const _PAYLOAD_LEN: u32 = usize_to_u32($payload_len);
        const $LEN: usize = 1 + u32_uleb_len(_PAYLOAD_LEN) + $payload_len;
        const fn $encoder(buffer: &mut [u8]) {
            debug_assert!(buffer.len() == $LEN);
            let (tag_buf, buffer) = buffer.split_at_mut(1);
            let (len_buf, payload_buffer) = buffer.split_at_mut(u32_uleb_len(_PAYLOAD_LEN));
            tag_buf[0] = $tag;
            u32_uleb(_PAYLOAD_LEN, len_buf);
            $encode_payload(payload_buffer);
        }
    };
}

// The user *could* link multiple versions of wasm-split, hence this
// subsection is not necessarily unique.

// Version 1: the marker only signals its presence. Its purpose is
// foward compatibility, as the cli finding an unrecognized larger
// version knows that it can't handle the input wasm and must be
// upgraded.
const VERSION: Version = Version::Version1;
const VERSION_PAYLOAD_LEN: usize = 1;
const fn encode_version(buffer: &mut [u8]) {
    debug_assert!(buffer.len() == VERSION_PAYLOAD_LEN);
    buffer[0] = VERSION as u8;
}

encode_subsection! {
    const (VERSION_SUBSECTION_LEN, fn encode_version_subsection) = (SubsectionTag::Version as u8, VERSION_PAYLOAD_LEN, encode_version);
}

const WASM_SPLIT_VERSION: [u8; VERSION_SUBSECTION_LEN] = {
    let mut buffer = [0; VERSION_SUBSECTION_LEN];
    encode_version_subsection(&mut buffer);
    buffer
};

// #[no_mangle] is needed so the linker doesn't remove the section. Might be replaced by #[used(linker)] in the future.
#[unsafe(no_mangle)]
#[unsafe(link_section = link_section!())]
static _WASM_SPLIT_VERSION: [u8; VERSION_SUBSECTION_LEN] = WASM_SPLIT_VERSION;
