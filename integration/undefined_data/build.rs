const WEAK_LINKAGE_PROBE: &str = r#"
extern "C" {
    #[linkage = "extern_weak"]
    static _OPTIONAL_VARIABLE: Option<&'static i32>;
}
"#;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let is_nightly = rustversion::cfg!(nightly);
    let ac = autocfg::new();
    let has_weak_linkage = ac.probe_raw(WEAK_LINKAGE_PROBE).is_ok();
    let has_weak_linkage_needs_feature = !has_weak_linkage
        && ac
            .probe_raw(&format!("#![feature(linkage)]\n{WEAK_LINKAGE_PROBE}"))
            .is_ok();
    assert!(
        !is_nightly || (has_weak_linkage || has_weak_linkage_needs_feature),
        "expected weak linkage to work on nightly"
    );

    autocfg::emit_possibility("has_weak_linkage");
    autocfg::emit_possibility("has_weak_linkage_needs_feature");
    if has_weak_linkage || has_weak_linkage_needs_feature {
        autocfg::emit("has_weak_linkage");
    }
    if has_weak_linkage_needs_feature {
        autocfg::emit("has_weak_linkage_needs_feature");
    }
}
