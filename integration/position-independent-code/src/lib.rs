#![cfg_attr(has_global_asm_needs_feature, feature(asm_experimental_arch))]

#[cfg(target_family = "wasm")]
#[cfg(has_global_asm)]
pub mod asm {
    std::arch::global_asm! {
        r#"
        .section .data.fizz,"",@
        .globl fizz
        fizz:
          .dc.l 0
          .size fizz, 4
        .section .data.bar,"",@
        .globl bar
        bar:
          .dc.a fizz - bar
          .size bar, bar - .
        "#
    }

    unsafe extern "C" {
        #[link_name = "fizz"]
        pub safe static FIZZ: u32;
        #[link_name = "bar"]
        safe static BAR: isize;
    }

    /// The address the linker resolved for the undefined data symbol.
    #[wasm_split_helpers::wasm_split(split)]
    pub fn read_pic_data() -> (isize, usize) {
        (BAR, std::ptr::from_ref(&BAR).addr())
    }
}

// This pulls in the magic marker which we assert in all tests
const _: () = {
    let _ = wasm_split_helpers::rt::ensure_loaded;
};

#[cfg(test)]
mod tests {
    #[allow(unused)]
    use wasm_bindgen_test::wasm_bindgen_test as test;

    #[cfg(target_family = "wasm")]
    #[cfg(has_global_asm)]
    #[test]
    async fn undefined_data_keeps_the_linker_address() {
        let (bar, bar_addr) = super::asm::read_pic_data().await;
        let fizz_addr = std::ptr::from_ref(&super::asm::FIZZ).addr();
        assert_eq!(bar, fizz_addr.wrapping_sub(bar_addr) as isize);
    }
}
