//! Builds a minimal splittable wasm whose symbol table carries an
//! *undefined* data symbol - `definition: None`, `WASM_SYM_UNDEFINED` -
//! that is referenced by a `R_WASM_MEMORY_ADDR_LEB` relocation from a
//! function kept in the main module. rustc produces this shape with
//! `incremental` + multiple codegen units: a reused object can keep
//! references to a promoted anonymous global (`anon.<hash>.<n>.llvm.<id>`)
//! whose content-derived name changed in a recompiled unit, and wasm-ld
//! links the dangling reference under `--allow-undefined`, resolving it
//! to address 0.
//!
//! Runs the full `wasm_split_cli_support::transform()` pipeline on it and
//! asserts:
//!
//!   1. the transform succeeds - the undefined symbol has no definition
//!      to place or relocate, so it must be skipped, not treated as
//!      fatal; and
//!   2. the reference to the *defined* data symbol next to it is still
//!      relocated to the output segment's base - proving the relocation
//!      plumbing in this test attaches to the dependency pass (a broken
//!      reloc encoding would make assertion 1 pass vacuously); and
//!   3. the reference to the undefined symbol keeps the address the
//!      linker resolved (0), untouched by any fixup.
//!
//! See #59.

use std::borrow::Cow;

use wasm_encoder::{
    CodeSection, ConstExpr, CustomSection, DataSection, DataSymbolDefinition, Encode, ExportKind,
    ExportSection, Function, FunctionSection, ImportSection, Instruction, MemorySection,
    MemoryType, Module, RefType, SymbolTable, TableSection, TableType, TypeSection,
};
use wasmparser::{DataKind, Operator, Parser, Payload, TypeRef};

use wasm_split_cli_support::{transform, Options};

#[path = "../src/magic_constants.rs"]
mod magic_constants;

/// Input layout (with 1 import): 0 = placeholder import, 1 = split_body,
/// 2 = main, 3 = reads_data.
const SPLIT_BODY_IDX: u32 = 1;
const MAIN_IDX: u32 = SPLIT_BODY_IDX + 1;
const READS_DATA_IDX: u32 = MAIN_IDX + 1;
const READS_DATA_EXPORT: &str = "reads_data";

/// Symbol table layout.
const DEFINED_DATA_SYM: u32 = 1;
const UNDEFINED_DATA_SYM: u32 = 2;

const SEGMENT_BASE: i32 = 1024;
const SEGMENT_BYTES: &[u8] = b"0123456789abcdef";

/// `i32.const` with a linker-style 5-byte padded LEB immediate, the shape
/// relocation fixups patch in place.
fn padded_i32_const(value: u32) -> [u8; 6] {
    let mut bytes = [0x41, 0x80, 0x80, 0x80, 0x80, 0x00];
    bytes[1] |= (value & 0x7f) as u8;
    bytes[2] |= ((value >> 7) & 0x7f) as u8;
    bytes[3] |= ((value >> 14) & 0x7f) as u8;
    bytes[4] |= ((value >> 21) & 0x7f) as u8;
    bytes[5] = ((value >> 28) & 0x0f) as u8;
    bytes
}

fn uleb(mut value: u64, out: &mut Vec<u8>) {
    loop {
        let byte = (value & 0x7f) as u8;
        value >>= 7;
        if value == 0 {
            out.push(byte);
            return;
        }
        out.push(byte | 0x80);
    }
}

/// The module, optionally with a `reloc.CODE` custom section appended.
/// Two-pass construction: the first pass (without relocations) is byte
/// identical up to the appended section, so offsets measured on it stay
/// valid for the second.
fn build_input_wasm(reloc_code: Option<&[u8]>) -> Vec<u8> {
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
        magic_constants::PLACEHOLDER_IMPORT_MODULE,
        &import_name,
        wasm_encoder::EntityType::Function(0),
    );
    module.section(&imports);

    let mut functions = FunctionSection::new();
    for _ in 1..=READS_DATA_IDX {
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

    let mut exports = ExportSection::new();
    exports.export(&export_name, ExportKind::Func, SPLIT_BODY_IDX);
    exports.export("main", ExportKind::Func, MAIN_IDX);
    exports.export(READS_DATA_EXPORT, ExportKind::Func, READS_DATA_IDX);
    exports.export("__indirect_function_table", ExportKind::Table, 0);
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);

    let mut code = CodeSection::new();

    let mut split_body = Function::new([]);
    split_body.instruction(&Instruction::End);
    code.function(&split_body);

    let mut main = Function::new([]);
    main.instruction(&Instruction::End);
    code.function(&main);

    // reads_data: two padded-LEB `i32.const` sites, one per relocation.
    // Raw bytes rather than the instruction API: the immediates must be
    // 5-byte padded LEBs for an in-place fixup, and the encoder emits
    // minimal ones.
    let mut reads_data = vec![0x00]; // no locals
    reads_data.extend_from_slice(&padded_i32_const(SEGMENT_BASE as u32));
    reads_data.push(0x1a); // drop
    reads_data.extend_from_slice(&padded_i32_const(0));
    reads_data.push(0x1a); // drop
    reads_data.push(0x0b); // end
    code.raw(&reads_data);

    module.section(&code);

    let mut data = DataSection::new();
    data.active(
        0,
        &ConstExpr::i32_const(SEGMENT_BASE),
        SEGMENT_BYTES.iter().copied(),
    );
    module.section(&data);

    let mut sym_tab = SymbolTable::new();
    sym_tab.table(
        SymbolTable::WASM_SYM_BINDING_LOCAL,
        0,
        Some("__indirect_function_table"),
    );
    // Symbol 1: an ordinary defined data symbol covering the segment.
    sym_tab.data(
        0,
        "defined_data",
        Some(DataSymbolDefinition {
            index: 0,
            offset: 0,
            size: SEGMENT_BYTES.len() as u32,
        }),
    );
    // Symbol 2: the undefined data symbol - no definition part. The name
    // mirrors what rustc incremental builds leave behind.
    sym_tab.data(
        SymbolTable::WASM_SYM_UNDEFINED,
        "anon.baadf00dcafe.7.llvm.1234567890",
        None,
    );
    // Hand-built "linking" custom section: wasm-encoder's LinkingSection
    // has no segment-info API yet, and the reader requires one entry per
    // data segment. Layout: version, WASM_SEGMENT_INFO subsection, then
    // the encoded symbol table subsection.
    let mut linking_data = Vec::new();
    uleb(2, &mut linking_data); // linking metadata version
    let mut segment_info = Vec::new();
    uleb(1, &mut segment_info); // one segment
    uleb(".data".len() as u64, &mut segment_info);
    segment_info.extend_from_slice(b".data");
    uleb(0, &mut segment_info); // alignment (log2)
    uleb(0, &mut segment_info); // flags
    linking_data.push(0x05); // WASM_SEGMENT_INFO
    uleb(segment_info.len() as u64, &mut linking_data);
    linking_data.extend_from_slice(&segment_info);
    sym_tab.encode(&mut linking_data);
    module.section(&CustomSection {
        name: Cow::Borrowed("linking"),
        data: Cow::Borrowed(&linking_data),
    });

    // wasm-split marker: [tag u8][payload_len uleb][payload]. (see wasm_split/../marker.rs)
    let ws_payload = [1u8, 1u8, 1u8];
    module.section(&CustomSection {
        name: Cow::Borrowed(magic_constants::LINK_SECTION),
        data: Cow::Borrowed(&ws_payload),
    });

    if let Some(payload) = reloc_code {
        module.section(&CustomSection {
            name: Cow::Borrowed("reloc.CODE"),
            data: Cow::Borrowed(payload),
        });
    }

    module.finish()
}

/// `(section_index, contents_range)` of the code section, counting section
/// indices the way the relocation reader does: one per section payload, in
/// module order.
fn locate_code_section(wasm: &[u8]) -> (u64, std::ops::Range<u64>) {
    let mut section_index: u64 = 0;
    for payload in Parser::new(0).parse_all(wasm) {
        let payload = payload.expect("valid wasm");
        if let Payload::CodeSectionStart { range, .. } = &payload {
            return (section_index, range.start as u64..range.end as u64);
        }
        if payload.as_section().is_some() {
            section_index += 1;
        }
    }
    panic!("module has no code section");
}

/// Byte offset of `pattern` in `haystack`; the pattern must be unique.
fn locate_unique(haystack: &[u8], pattern: &[u8]) -> u64 {
    let mut positions = haystack
        .windows(pattern.len())
        .enumerate()
        .filter(|(_, w)| *w == pattern)
        .map(|(i, _)| i as u64);
    let position = positions.next().expect("pattern not found");
    assert!(positions.next().is_none(), "pattern is not unique");
    position
}

/// `(func_export_index, defined_func_index)`.
fn locate_export(wasm: &[u8], export_name: &str) -> (u32, usize) {
    let mut import_count: usize = 0;
    let mut target: Option<u32> = None;
    for payload in Parser::new(0).parse_all(wasm) {
        match payload.expect("valid wasm") {
            Payload::ImportSection(reader) => {
                for imp in reader {
                    let Ok(wasmparser::Imports::Single(_, imp)) = imp else {
                        panic!("valid import")
                    };
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
                        break;
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
    (idx, defined)
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

/// The immediates of every `i32.const` in a body, in order.
fn i32_const_immediates(body: &wasmparser::FunctionBody<'_>) -> Vec<i32> {
    let mut reader = body.get_operators_reader().expect("operators");
    let mut values = Vec::new();
    while !reader.eof() {
        if let Operator::I32Const { value } = reader.read().expect("op") {
            values.push(value);
        }
    }
    values
}

/// Base address of the module's single active data segment.
fn single_segment_base(wasm: &[u8]) -> i32 {
    let mut base: Option<i32> = None;
    for payload in Parser::new(0).parse_all(wasm) {
        if let Payload::DataSection(reader) = payload.expect("valid wasm") {
            for segment in reader {
                let segment = segment.expect("valid data segment");
                let DataKind::Active { offset_expr, .. } = segment.kind else {
                    panic!("expected an active data segment");
                };
                let mut ops = offset_expr.get_operators_reader();
                let Operator::I32Const { value } = ops.read().expect("op") else {
                    panic!("expected an i32.const segment offset");
                };
                assert!(base.is_none(), "expected exactly one data segment");
                base = Some(value);
            }
        }
    }
    base.expect("output module has no data segment - did the relocations attach?")
}

#[test]
fn undefined_data_symbol_is_skipped_not_fatal() {
    tracing_subscriber::fmt::init();

    // Pass 1: measure the relocation targets on a module without the
    // reloc section (appending it does not shift earlier offsets).
    let unrelocated = build_input_wasm(None);
    let (code_section_index, code_range) = locate_code_section(&unrelocated);
    let defined_site =
        locate_unique(&unrelocated, &padded_i32_const(SEGMENT_BASE as u32)) + 1 - code_range.start;
    let undefined_site = locate_unique(&unrelocated, &padded_i32_const(0)) + 1 - code_range.start;

    // R_WASM_MEMORY_ADDR_LEB entries against the defined and the
    // undefined data symbol.
    let mut reloc = Vec::new();
    uleb(code_section_index, &mut reloc);
    uleb(2, &mut reloc);
    for (site, symbol) in [
        (defined_site, DEFINED_DATA_SYM),
        (undefined_site, UNDEFINED_DATA_SYM),
    ] {
        reloc.push(0x03); // R_WASM_MEMORY_ADDR_LEB
        uleb(site, &mut reloc);
        uleb(u64::from(symbol), &mut reloc);
        reloc.push(0x00); // addend 0
    }

    let input = build_input_wasm(Some(&reloc));

    // Sanity: input should parse as a wasm module.
    for payload in Parser::new(0).parse_all(&input) {
        payload.expect("input wasm is valid");
    }

    let mut tmp = tempfile::tempdir().expect("create tmpdir");
    tmp.disable_cleanup(true);
    let main_out = tmp.path().join("main.wasm");

    let mut opts = Options::new(&input);
    opts.output_dir = tmp.path();
    opts.main_out_path = &main_out;

    // --- Assertion 1: the undefined data symbol must not be fatal.
    transform(opts).expect("transform succeeds despite the undefined data symbol");

    let main_bytes = std::fs::read(&main_out).expect("read main.wasm output");
    let (_, reads_data_idx) = locate_export(&main_bytes, READS_DATA_EXPORT);
    let immediates = i32_const_immediates(&nth_defined_body(&main_bytes, reads_data_idx));
    let [defined_imm, undefined_imm] = immediates[..] else {
        panic!("expected two i32.const sites in reads_data, got {immediates:?}");
    };

    // --- Assertion 2: the defined symbol's site was relocated to the
    // output segment. This is the canary that the reloc.CODE section in
    // this test really attaches to the dependency pass: without it, the
    // output would carry no data segment at all and assertion 1 would
    // pass vacuously.
    let segment_base = single_segment_base(&main_bytes);
    assert_eq!(
        defined_imm, segment_base,
        "the defined data symbol's reference should point at the output \
         segment base {segment_base}, got {defined_imm}",
    );

    // --- Assertion 3: the undefined symbol's site keeps the address the
    // linker resolved. There is no definition whose relocation could
    // change it.
    assert_eq!(
        undefined_imm, 0,
        "the undefined data symbol's reference must stay at the \
         linker-resolved address 0, got {undefined_imm}",
    );

    tmp.disable_cleanup(false);
}
