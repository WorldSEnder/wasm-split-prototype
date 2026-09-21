const GLOBAL_ASM_PROBE: &str = r#"
std::arch::global_asm!{""}
"#;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let is_nightly = rustversion::cfg!(nightly);
    let ac = autocfg::new();
    let has_global_asm = ac.probe_raw(GLOBAL_ASM_PROBE).is_ok();
    let has_global_asm_needs_feature = !has_global_asm
        && ac
            .probe_raw(&format!(
                "#![feature(asm_experimental_arch)]\n{GLOBAL_ASM_PROBE}"
            ))
            .is_ok();
    assert!(
        !is_nightly || (has_global_asm || has_global_asm_needs_feature),
        "expected global asm to work on nightly"
    );

    autocfg::emit_possibility("has_global_asm");
    autocfg::emit_possibility("has_global_asm_needs_feature");
    if has_global_asm || has_global_asm_needs_feature {
        autocfg::emit("has_global_asm");
    }
    if has_global_asm_needs_feature {
        autocfg::emit("has_global_asm_needs_feature");
    }
}
