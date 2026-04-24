//! Builds a minimal splittable wasm that contains a function whose `call`
//! instruction has no covering `reloc.CODE` entry — the exact pattern
//! wasm-ld synthesizes for LTO-merged wrapper stubs. Runs the full
//! `wasm_split_cli_support::transform()` pipeline on it and asserts:
//!
//!   1. a pure call-forwarding `stub` (body = `call $helper; end`) gets
//!      re-encoded, so its `call` immediate in the output points at
//!      helper's remapped index (not the stale input-module index); and
//!   2. a `dangerous_stub` (body = `global.get $g; drop; call $helper`)
//!      is rejected by the warn list and falls through to the raw-copy
//!      path — its `call` immediate in the output MUST stay at the
//!      stale input index, because re-encoding it would silently emit
//!      a stale `global.get` immediate (FuncReencoder only remaps
//!      function indices).
//!
//! See #29.

use std::borrow::Cow;

use wasm_encoder::{
    CodeSection, ConstExpr, CustomSection, ExportKind, ExportSection, Function, FunctionSection,
    GlobalSection, GlobalType, ImportSection, Instruction, LinkingSection, MemorySection,
    MemoryType, Module, RefType, SymbolTable, TableSection, TableType, TypeSection, ValType,
};
use wasmparser::{Operator, Parser, Payload, TypeRef};

use wasm_split_cli_support::{transform, Options};

/// Input layout (with 1 import): 0 = placeholder import, 1..=10 = padding,
/// 11 = helper, 12 = stub, 13 = dangerous_stub, 14 = split_body, 15 = main.
///
/// Padding pushes helper to a HIGH input index so that a stale `call 11`
/// in the output can never coincide with the remapped output index (the
/// main output module only keeps a handful of defined funcs).
const PAD: u32 = 10;
const HELPER_IDX: u32 = 1 + PAD;
const STUB_IDX: u32 = HELPER_IDX + 1;
const DANGEROUS_STUB_IDX: u32 = STUB_IDX + 1;
const SPLIT_BODY_IDX: u32 = DANGEROUS_STUB_IDX + 1;
const MAIN_IDX: u32 = SPLIT_BODY_IDX + 1;

/// Crucially there is **no `reloc.CODE` custom section**. That means
/// every `call` instruction in defined functions has no reloc entry —
/// the exact condition that used to miscompile before PR #29.
fn build_input_wasm() -> Vec<u8> {
    let mut module = Module::new();

    let mut types = TypeSection::new();
    types.ty().function([], []);
    module.section(&types);

    const SPLIT_NAME: &str = "testsplit";
    const SPLIT_HASH: &str = "00000000000000000000000000000000";
    let import_name = format!("__wasm_split_00{SPLIT_NAME}00_import_{SPLIT_HASH}");
    let export_name = format!("__wasm_split_00{SPLIT_NAME}00_export_{SPLIT_HASH}");

    let mut imports = ImportSection::new();
    imports.import(
        "__wasm_split_placeholder__",
        &import_name,
        wasm_encoder::EntityType::Function(0),
    );
    module.section(&imports);

    let mut functions = FunctionSection::new();
    for _ in 0..(PAD + 5) {
        functions.function(0);
    }
    module.section(&functions);

    let mut tables = TableSection::new();
    tables.table(TableType {
        element_type: RefType::FUNCREF,
        minimum: 1,
        maximum: Some(1),
        table64: false,
        shared: false,
    });
    module.section(&tables);

    let mut memories = MemorySection::new();
    memories.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memories);

    // One i32 global, referenced by dangerous_stub's `global.get 0`.
    let mut globals = GlobalSection::new();
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(0),
    );
    module.section(&globals);

    let mut exports = ExportSection::new();
    exports.export(&export_name, ExportKind::Func, SPLIT_BODY_IDX);
    exports.export("main", ExportKind::Func, MAIN_IDX);
    exports.export("stub", ExportKind::Func, STUB_IDX);
    exports.export("dangerous_stub", ExportKind::Func, DANGEROUS_STUB_IDX);
    exports.export("helper", ExportKind::Func, HELPER_IDX);
    exports.export("__indirect_function_table", ExportKind::Table, 0);
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);

    let mut code = CodeSection::new();

    // Padding: empty bodies, no calls → silently ignored by the classifier.
    for _ in 0..PAD {
        let mut pad = Function::new([]);
        pad.instruction(&Instruction::End);
        code.function(&pad);
    }

    // helper: empty body — not a stub (no `call` inside).
    let mut helper = Function::new([]);
    helper.instruction(&Instruction::End);
    code.function(&helper);

    // stub: pure `call $helper` + end. Classifier must detect this and
    // re-encode it, rewriting the call immediate to helper's output idx.
    let mut stub = Function::new([]);
    stub.instruction(&Instruction::Call(HELPER_IDX));
    stub.instruction(&Instruction::End);
    code.function(&stub);

    // dangerous_stub: `global.get 0; drop; call $helper`. The classifier
    // must see `global.get` and refuse to treat this as a re-encodable
    // stub (FuncReencoder only remaps function indices, not globals).
    // It should fall through to the raw-copy emit path, preserving the
    // body bytes verbatim — including the stale input `call $helper`
    // immediate.
    let mut dangerous = Function::new([]);
    dangerous.instruction(&Instruction::GlobalGet(0));
    dangerous.instruction(&Instruction::Drop);
    dangerous.instruction(&Instruction::Call(HELPER_IDX));
    dangerous.instruction(&Instruction::End);
    code.function(&dangerous);

    // split_body: reachable only from the split-point export, ends up in
    // the split module.
    let mut split_body = Function::new([]);
    split_body.instruction(&Instruction::Call(HELPER_IDX));
    split_body.instruction(&Instruction::End);
    code.function(&split_body);

    // main: keeps both stubs reachable in the main module.
    let mut main = Function::new([]);
    main.instruction(&Instruction::Call(STUB_IDX));
    main.instruction(&Instruction::Call(DANGEROUS_STUB_IDX));
    main.instruction(&Instruction::End);
    code.function(&main);

    module.section(&code);

    let mut linking = LinkingSection::new();
    let mut sym_tab = SymbolTable::new();
    sym_tab.table(
        SymbolTable::WASM_SYM_BINDING_LOCAL,
        0,
        Some("__indirect_function_table"),
    );
    linking.symbol_table(&sym_tab);
    module.section(&linking);

    // wasm-split marker: [tag u8][payload_len uleb][payload].
    let ws_payload = [1u8, 1u8, 1u8];
    module.section(&CustomSection {
        name: Cow::Borrowed("__wasm_split_unstable"),
        data: Cow::Borrowed(&ws_payload),
    });

    module.finish()
}

/// `(func_export_index, defined_func_index, import_count)`.
fn locate_export(wasm: &[u8], export_name: &str) -> (u32, usize, usize) {
    let mut import_count: usize = 0;
    let mut target: Option<u32> = None;
    for payload in Parser::new(0).parse_all(wasm) {
        match payload.expect("valid wasm") {
            Payload::ImportSection(reader) => {
                for imp in reader {
                    let imp = imp.expect("valid import");
                    if matches!(imp.ty, TypeRef::Func(_)) {
                        import_count += 1;
                    }
                }
            }
            Payload::ExportSection(reader) => {
                for exp in reader {
                    let exp = exp.expect("valid export");
                    if exp.name == export_name {
                        target = Some(exp.index);
                    }
                }
            }
            _ => {}
        }
    }
    let idx = target.unwrap_or_else(|| panic!("export {export_name:?} not found"));
    let defined = (idx as usize).checked_sub(import_count).unwrap_or_else(|| {
        panic!(
            "export {export_name:?} points at imported index {idx}, \
             not a defined function (imports = {import_count})"
        )
    });
    (idx, defined, import_count)
}

/// Returns the first `Call`'s `function_index` immediate from a body.
fn first_call_immediate(body: &wasmparser::FunctionBody<'_>) -> u32 {
    let mut reader = body.get_operators_reader().expect("operators");
    while !reader.eof() {
        if let Operator::Call { function_index } = reader.read().expect("op") {
            return function_index;
        }
    }
    panic!("body contained no Call operator");
}

fn nth_defined_body<'a>(wasm: &'a [u8], nth: usize) -> wasmparser::FunctionBody<'a> {
    Parser::new(0)
        .parse_all(wasm)
        .filter_map(|p| match p.expect("valid wasm") {
            Payload::CodeSectionEntry(body) => Some(body),
            _ => None,
        })
        .nth(nth)
        .unwrap_or_else(|| panic!("module has fewer than {} defined functions", nth + 1))
}

#[test]
fn no_reloc_stub_is_rewritten_and_dangerous_stub_is_raw_copied() {
    let input = build_input_wasm();

    // Sanity: input should parse as a wasm module.
    for payload in Parser::new(0).parse_all(&input) {
        payload.expect("input wasm is valid");
    }

    let tmp = std::env::temp_dir().join(format!(
        "wasm_split_no_reloc_test_{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir_all(&tmp).expect("create tmpdir");
    let main_out = tmp.join("main.wasm");

    let mut opts = Options::new(&input);
    opts.output_dir = &tmp;
    opts.main_out_path = &main_out;
    opts.link_name = "./__wasm_split.js";
    opts.main_module = "./main.js";

    transform(opts).expect("transform succeeds");

    let main_bytes = std::fs::read(&main_out).expect("read main.wasm output");

    let (helper_func_idx, _, _) = locate_export(&main_bytes, "helper");
    let (_, stub_defined_idx, _) = locate_export(&main_bytes, "stub");
    let (_, dangerous_defined_idx, _) = locate_export(&main_bytes, "dangerous_stub");

    // --- Assertion 1: pure stub's call was remapped.
    let stub_call = first_call_immediate(&nth_defined_body(&main_bytes, stub_defined_idx));
    assert_eq!(
        stub_call, helper_func_idx,
        "pure stub should have its `call` immediate remapped to helper's \
         output index {helper_func_idx}, got {stub_call} — PR #29 remap regressed?",
    );

    // --- Assertion 2: dangerous stub was raw-copied (NOT re-encoded).
    // A stale input immediate is the signal that the warn list rejected
    // the classification and the body went through raw-copy. Commenting
    // out the warn list flips this: the dangerous stub gets re-encoded
    // and the call is rewritten to `helper_func_idx` — silently emitting
    // a stale `global.get` immediate in the process.
    let dangerous_call =
        first_call_immediate(&nth_defined_body(&main_bytes, dangerous_defined_idx));

    assert_eq!(
        dangerous_call, HELPER_IDX,
        "dangerous stub (with global.get) must be raw-copied, so its \
         `call` immediate stays at the stale input index {HELPER_IDX}. \
         Got {dangerous_call} — warn-list regressed and the body went \
         through the re-encoder, which would also silently mis-emit the \
         global.get immediate.",
    );

    let _ = std::fs::remove_dir_all(&tmp);
}
