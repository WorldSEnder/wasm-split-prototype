//! Builds minimal splittable wasm modules whose data segment holds symbols
//! that share bytes - the layout wasm-ld produces when it tail-merges strings
//! while optimizing - and runs the full `wasm_split_cli_support::transform()`
//! pipeline on them.
//! `shared_data_symbols_are_emitted_once_and_split_data_is_relocated`: the
//! split module uses a whole string, the main module a symbol for its tail.
//! Asserts that the shared bytes are emitted exactly once, from the main
//! module, that both functions refer to that one copy, and that the bytes
//! only the split module uses are relocated into the split module.
//!
//! Before the fix, the shared bytes were allocated once per module, the
//! segment became longer than its input, and the *whole* segment was put into
//! the main module, so nothing was ever relocated into split modules of
//! release builds (leptos-rs/cargo-leptos#679).
//!
//! `overlapping_symbols_keep_the_strictest_alignment`: a 4-aligned word is
//! overlapped by a longer, unaligned string. Asserts that the merged bytes
//! are placed so that the word stays aligned.
//!
//! `pointer_inside_a_contained_symbol_pulls_its_target_into_main`: the main
//! module uses the tail of a split's symbol, and that tail holds a pointer.
//! Asserts that the pointer's target is available to main.
//!
//! `relocation_crossing_an_inner_symbol_belongs_to_the_containing_symbol`: a
//! pointer straddles the boundary of a symbol nested in another. Asserts that
//! it is attributed to the containing symbol instead of being rejected.
//!
//! `data_shared_between_splits_is_emitted_from_their_chunk`: split `a` uses a
//! whole string, splits `a` and `b` both use its tail. Asserts that the whole
//! string is emitted from the chunk shared by `a` and `b`, not from main.
//!
//! `chunk_placement_uses_the_exact_chunk_of_all_requiring_splits`: a split's
//! symbol contains three symbols shared with other splits, so the merged range
//! is required by a growing set of splits. Asserts that the range ends up in
//! the chunk shared by exactly the final set, even though no chunk matched an
//! intermediate set.
//!
//! `chunk_placement_records_every_requiring_split`: like the previous one, but
//! one of the contained symbols belongs to the chunk provisionally chosen for
//! an intermediate set. Asserts that its splits are still accounted for, so
//! the range does not end up in a chunk one of them never loads.
//!
//! `mixed_alignments_pack_without_padding_across_modules`: modules mixing
//! aligned words with bytes. Asserts that the data packs without padding by
//! emitting one segment per alignment, that every input segment keeps its
//! index in the outputs and the extra segments are appended.
//!
//! `overlong_segment_folds_the_smallest_split_into_main`: a merged range with
//! an unaligned start makes the relocated segment longer than its input.
//! Asserts that only the smallest split's data moves into main and the other
//! split keeps its relocated data, instead of the whole segment being copied
//! into main unrelocated.
//!
//! `overlapping_input_segments_keep_their_order`: two input segments overlap
//! in memory, which an appended fragment could reorder. Asserts that both are
//! kept as they are, in their slots.

use std::borrow::Cow;

use wasm_encoder::{
    CodeSection, ConstExpr, CustomSection, DataSection, Encode, ExportKind, ExportSection,
    Function, FunctionSection, ImportSection, Instruction, MemorySection, MemoryType, Module,
    RefType, TableSection, TableType, TypeSection, ValType,
};
use wasmparser::{DataKind, Operator, Parser, Payload};

use wasm_split_cli_support::{transform, Options};

#[path = "../src/magic_constants.rs"]
mod magic_constants;

const SPLIT_HASH: &str = "00000000000000000000000000000000";

/// Where the input's only data segment is placed in memory.
const SEGMENT_BASE: u32 = 1024;
/// The whole string; other symbols use its tail.
const SHARED: &[u8] = b"shared-bytes\0";
const SHARED_TAIL_OFFSET: usize = 7; // "bytes\0"
/// Bytes only one module refers to.
const SPLIT_ONLY: &[u8] = b"split-only\0";
const MAIN_ONLY: &[u8] = b"main-only\0";

/// `R_WASM_MEMORY_ADDR_LEB`
const R_WASM_MEMORY_ADDR_LEB: u8 = 3;
/// `R_WASM_MEMORY_ADDR_I32`
const R_WASM_MEMORY_ADDR_I32: u8 = 5;

/// A defined data symbol: `(name, offset in the segment, size)`.
struct DataSymbol(&'static str, usize, usize);

/// Which module a function belongs to.
enum Owner {
    /// Exported from the main module under this name.
    Main(&'static str),
    /// The entry of the split with this name.
    Split(&'static str),
}

/// A defined function that refers to data symbols (by index into
/// [`Input::symbols`]) through `i32.const` immediates, in this order.
struct Func(Owner, Vec<usize>);

struct Input {
    data: Vec<u8>,
    /// log2 of the segment's alignment
    alignment: u32,
    symbols: Vec<DataSymbol>,
    /// Further data segments: `(address, bytes, symbols relative to the segment)`. Their
    /// symbols are numbered after [`Input::symbols`], in order.
    extra_segments: Vec<(u32, Vec<u8>, Vec<DataSymbol>)>,
    funcs: Vec<Func>,
    /// Pointers inside the data: `(offset in the segment, index of the symbol pointed to)`.
    /// The builder writes the input address of the symbol there.
    data_relocs: Vec<(usize, usize)>,
}

fn uleb(mut value: u32, out: &mut Vec<u8>) {
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

fn name(s: &str, out: &mut Vec<u8>) {
    uleb(s.len() as u32, out);
    out.extend_from_slice(s.as_bytes());
}

/// `i32.const` with a 5-byte padded immediate, as emitted for relocatable code.
fn padded_i32_const(mut value: u32) -> [u8; 6] {
    let mut bytes = [0x41, 0, 0, 0, 0, 0];
    for byte in &mut bytes[1..5] {
        *byte = (value & 0x7f) as u8 | 0x80;
        value >>= 7;
    }
    bytes[5] = (value & 0x7f) as u8;
    bytes
}

fn split_import_name(split: &str) -> String {
    format!("__wasm_split_00{split}00_import_{SPLIT_HASH}")
}

fn split_export_name(split: &str) -> String {
    format!("__wasm_split_00{split}00_export_{SPLIT_HASH}")
}

impl Input {
    /// All symbols as `(segment index, symbol)`, in symbol table order.
    fn all_symbols(&self) -> Vec<(usize, &DataSymbol)> {
        let mut symbols: Vec<_> = self.symbols.iter().map(|symbol| (0, symbol)).collect();
        for (i, (_, _, segment_symbols)) in self.extra_segments.iter().enumerate() {
            symbols.extend(segment_symbols.iter().map(|symbol| (1 + i, symbol)));
        }
        symbols
    }

    /// The input address of a symbol, by index into [`Input::all_symbols`].
    fn symbol_address(&self, symbol: usize) -> u32 {
        let (segment, DataSymbol(_, offset, _)) = self.all_symbols()[symbol];
        let base = match segment {
            0 => SEGMENT_BASE,
            i => self.extra_segments[i - 1].0,
        };
        base + *offset as u32
    }

    fn splits(&self) -> Vec<&'static str> {
        self.funcs
            .iter()
            .filter_map(|Func(owner, _)| match owner {
                Owner::Split(split) => Some(*split),
                Owner::Main(_) => None,
            })
            .collect()
    }

    /// Everything but the linking metadata. The metadata is appended
    /// afterwards, which keeps the code offsets computed from this prefix valid.
    fn build_module_prefix(&self) -> Vec<u8> {
        let splits = self.splits();
        let mut module = Module::new();

        let mut types = TypeSection::new();
        types.ty().function([], [ValType::I32]);
        module.section(&types);

        // One placeholder import per split, then the defined functions.
        let mut imports = ImportSection::new();
        for split in &splits {
            imports.import(
                magic_constants::PLACEHOLDER_IMPORT_MODULE,
                &split_import_name(split),
                wasm_encoder::EntityType::Function(0),
            );
        }
        module.section(&imports);

        let mut functions = FunctionSection::new();
        for _ in &self.funcs {
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
        for (i, Func(owner, _)) in self.funcs.iter().enumerate() {
            let func_index = (splits.len() + i) as u32;
            let export_name = match owner {
                Owner::Main(export) => export.to_string(),
                Owner::Split(split) => split_export_name(split),
            };
            exports.export(&export_name, ExportKind::Func, func_index);
        }
        exports.export("__indirect_function_table", ExportKind::Table, 0);
        exports.export("memory", ExportKind::Memory, 0);
        module.section(&exports);

        let mut code = CodeSection::new();
        for Func(_, refs) in &self.funcs {
            // `i32.const a; drop; ...; i32.const z; end`: refers to every symbol, returns the last.
            let mut func = Function::new([]);
            for (i, &symbol) in refs.iter().enumerate() {
                if i != 0 {
                    func.instruction(&Instruction::Drop);
                }
                func.raw(padded_i32_const(self.symbol_address(symbol)));
            }
            func.instruction(&Instruction::End);
            code.function(&func);
        }
        module.section(&code);

        let mut data = DataSection::new();
        let mut bytes = self.data.clone();
        for &(offset, symbol) in &self.data_relocs {
            let address = self.symbol_address(symbol);
            bytes[offset..offset + 4].copy_from_slice(&address.to_le_bytes());
        }
        data.active(0, &ConstExpr::i32_const(SEGMENT_BASE as i32), bytes);
        for (address, bytes, _) in &self.extra_segments {
            data.active(0, &ConstExpr::i32_const(*address as i32), bytes.clone());
        }
        module.section(&data);

        module.finish()
    }

    fn build_wasm(&self) -> Vec<u8> {
        let mut wasm = self.build_module_prefix();
        let (code_section_index, immediates) = locate_code_immediates(&wasm);
        let (data_section_index, data_start) = locate_data_segment(&wasm);
        let reloc_symbols: Vec<u32> = self
            .funcs
            .iter()
            .flat_map(|Func(_, refs)| refs.iter().map(|&symbol| 1 + symbol as u32))
            .collect();
        assert_eq!(immediates.len(), reloc_symbols.len());

        // "linking" section, version 2
        let mut linking = vec![];
        uleb(2, &mut linking);
        {
            // WASM_SEGMENT_INFO
            let mut payload = vec![];
            uleb(1 + self.extra_segments.len() as u32, &mut payload);
            for _ in 0..=self.extra_segments.len() {
                name(".rodata", &mut payload);
                uleb(self.alignment, &mut payload); // alignment (log2)
                uleb(0, &mut payload); // flags
            }
            linking.push(5);
            uleb(payload.len() as u32, &mut linking);
            linking.extend_from_slice(&payload);
        }
        {
            // WASM_SYMBOL_TABLE
            const SYMTAB_DATA: u8 = 1;
            const SYMTAB_TABLE: u8 = 5;
            const WASM_SYM_BINDING_LOCAL: u32 = 2;
            let all_symbols = self.all_symbols();
            let mut payload = vec![];
            uleb(1 + all_symbols.len() as u32, &mut payload);
            // 0: the indirect function table
            payload.push(SYMTAB_TABLE);
            uleb(WASM_SYM_BINDING_LOCAL, &mut payload);
            uleb(0, &mut payload);
            name("__indirect_function_table", &mut payload);
            // 1..: data symbols, `(segment, offset, size)`
            for (segment, DataSymbol(symbol_name, offset, size)) in all_symbols {
                payload.push(SYMTAB_DATA);
                uleb(WASM_SYM_BINDING_LOCAL, &mut payload);
                name(symbol_name, &mut payload);
                uleb(segment as u32, &mut payload);
                uleb(*offset as u32, &mut payload);
                uleb(*size as u32, &mut payload);
            }
            linking.push(8);
            uleb(payload.len() as u32, &mut linking);
            linking.extend_from_slice(&payload);
        }
        wasm.push(0);
        CustomSection {
            name: Cow::Borrowed("linking"),
            data: Cow::Owned(linking),
        }
        .encode(&mut wasm);

        // wasm-split marker: [tag u8][payload_len uleb][payload]. (see wasm_split/../marker.rs)
        let ws_payload = [1u8, 1u8, 1u8];
        wasm.push(0);
        CustomSection {
            name: Cow::Borrowed(magic_constants::LINK_SECTION),
            data: Cow::Borrowed(&ws_payload),
        }
        .encode(&mut wasm);

        // "reloc.CODE": one entry per `i32.const` immediate
        let mut relocs = vec![];
        uleb(code_section_index, &mut relocs);
        uleb(immediates.len() as u32, &mut relocs);
        for (offset, symbol) in immediates.into_iter().zip(reloc_symbols) {
            relocs.push(R_WASM_MEMORY_ADDR_LEB);
            uleb(offset, &mut relocs);
            uleb(symbol, &mut relocs);
            relocs.push(0); // addend
        }
        wasm.push(0);
        CustomSection {
            name: Cow::Borrowed("reloc.CODE"),
            data: Cow::Owned(relocs),
        }
        .encode(&mut wasm);

        // "reloc.DATA": one entry per pointer inside the data
        if !self.data_relocs.is_empty() {
            let mut relocs = vec![];
            uleb(data_section_index, &mut relocs);
            uleb(self.data_relocs.len() as u32, &mut relocs);
            for &(offset, symbol) in &self.data_relocs {
                relocs.push(R_WASM_MEMORY_ADDR_I32);
                uleb(data_start + offset as u32, &mut relocs);
                uleb(1 + symbol as u32, &mut relocs);
                relocs.push(0); // addend
            }
            wasm.push(0);
            CustomSection {
                name: Cow::Borrowed("reloc.DATA"),
                data: Cow::Owned(relocs),
            }
            .encode(&mut wasm);
        }

        for payload in Parser::new(0).parse_all(&wasm) {
            payload.expect("input wasm is valid");
        }
        wasm
    }
}

/// `(section index of the code section, code relocation offsets of every
/// `i32.const` immediate, in function order)`.
fn locate_code_immediates(wasm: &[u8]) -> (u32, Vec<u32>) {
    let mut section_index: u32 = 0;
    let mut code_section: Option<(u32, u64)> = None;
    let mut immediates = vec![];
    for payload in Parser::new(0).parse_all(wasm) {
        let payload = payload.expect("valid wasm");
        if let Some((_, range)) = payload.as_section() {
            if let Payload::CodeSectionStart { .. } = payload {
                code_section = Some((section_index, range.start));
            }
            section_index += 1;
        }
        if let Payload::CodeSectionEntry(body) = payload {
            let (_, content_start) = code_section.expect("code section starts before entries");
            let mut ops = body.get_operators_reader().expect("operators");
            while !ops.eof() {
                let (op, offset) = ops.read_with_offset().expect("op");
                if let Operator::I32Const { .. } = op {
                    // the immediate follows the one-byte opcode
                    immediates.push((offset + 1 - content_start) as u32);
                }
            }
        }
    }
    let (index, _) = code_section.expect("code section");
    (index, immediates)
}

/// `(section index of the data section, data relocation offset of the first
/// byte of the only data segment)`.
fn locate_data_segment(wasm: &[u8]) -> (u32, u32) {
    let mut section_index: u32 = 0;
    for payload in Parser::new(0).parse_all(wasm) {
        let payload = payload.expect("valid wasm");
        let Some((_, range)) = payload.as_section() else {
            continue;
        };
        if let Payload::DataSection(reader) = &payload {
            let segment = reader
                .clone()
                .into_iter()
                .next()
                .expect("a data segment")
                .expect("valid data segment");
            let data_start = segment.range.end - segment.data.len() as u64;
            return (section_index, (data_start - range.start) as u32);
        }
        section_index += 1;
    }
    panic!("no data section");
}

/// The output modules of `transform` on an [`Input`].
struct Output {
    main: Vec<u8>,
    /// by split name
    splits: Vec<(String, Vec<u8>)>,
    chunks: Vec<Vec<u8>>,
}

fn split(input: &Input) -> Output {
    let wasm = input.build_wasm();

    let mut tmp = tempfile::tempdir().expect("create tmpdir");
    tmp.disable_cleanup(true);
    let main_out = tmp.path().join("main.wasm");

    let mut opts = Options::new(&wasm);
    opts.output_dir = tmp.path();
    opts.main_out_path = &main_out;

    let output = transform(opts).expect("transform succeeds");

    let main = std::fs::read(&main_out).expect("read main.wasm output");
    let mut splits = vec![];
    let mut chunks = vec![];
    for path in &output.split_modules {
        let file_name = path.file_name().unwrap().to_string_lossy();
        let bytes = std::fs::read(path).expect("read output module");
        if let Some(split) = file_name
            .strip_prefix("split_")
            .and_then(|rest| rest.strip_suffix(".wasm"))
        {
            splits.push((split.to_string(), bytes));
        } else if file_name.starts_with("chunk_") {
            chunks.push(bytes);
        } else {
            panic!("unexpected output module {file_name}");
        }
    }

    tmp.disable_cleanup(false);
    Output {
        main,
        splits,
        chunks,
    }
}

impl Output {
    fn split(&self, name: &str) -> &[u8] {
        &self
            .splits
            .iter()
            .find(|(split, _)| split == name)
            .unwrap_or_else(|| panic!("no split module {name:?}"))
            .1
    }
}

/// The `(address, bytes)` of every non-empty active data segment.
fn data_segments(wasm: &[u8]) -> Vec<(u32, Vec<u8>)> {
    let mut segments = vec![];
    for payload in Parser::new(0).parse_all(wasm) {
        let Payload::DataSection(reader) = payload.expect("valid wasm") else {
            continue;
        };
        for segment in reader {
            let segment = segment.expect("valid data segment");
            if segment.data.is_empty() {
                continue;
            }
            let DataKind::Active { offset_expr, .. } = segment.kind else {
                panic!("expected active data segments only");
            };
            let Operator::I32Const { value } = offset_expr
                .get_operators_reader()
                .read()
                .expect("offset expression")
            else {
                panic!("expected a constant segment offset");
            };
            segments.push((value as u32, segment.data.to_vec()));
        }
    }
    segments
}

/// A data segment's `(address, bytes)`, or `None` if it is empty.
type MaybeSegment = Option<(u32, Vec<u8>)>;

/// Every data segment of a module in order, and the data count.
fn all_data_segments(wasm: &[u8]) -> (Vec<MaybeSegment>, u32) {
    let mut segments = vec![];
    let mut count = None;
    for payload in Parser::new(0).parse_all(wasm) {
        match payload.expect("valid wasm") {
            Payload::DataCountSection { count: c, .. } => count = Some(c),
            Payload::DataSection(reader) => {
                for segment in reader {
                    let segment = segment.expect("valid data segment");
                    let DataKind::Active { offset_expr, .. } = segment.kind else {
                        panic!("expected active data segments only");
                    };
                    let Operator::I32Const { value } = offset_expr
                        .get_operators_reader()
                        .read()
                        .expect("offset expression")
                    else {
                        panic!("expected a constant segment offset");
                    };
                    segments.push(
                        (!segment.data.is_empty()).then(|| (value as u32, segment.data.to_vec())),
                    );
                }
            }
            _ => {}
        }
    }
    (segments, count.expect("a data count section"))
}

/// The one non-empty data segment of a module.
fn single_data_segment(wasm: &[u8]) -> (u32, Vec<u8>) {
    let mut segments = data_segments(wasm);
    assert_eq!(
        segments.len(),
        1,
        "expected exactly one non-empty data segment, got {segments:?}"
    );
    segments.pop().unwrap()
}

/// The `i32.const` immediates of every defined function, in function order.
fn function_constants(wasm: &[u8]) -> Vec<Vec<u32>> {
    let mut functions = vec![];
    for payload in Parser::new(0).parse_all(wasm) {
        let Payload::CodeSectionEntry(body) = payload.expect("valid wasm") else {
            continue;
        };
        let mut constants = vec![];
        let mut ops = body.get_operators_reader().expect("operators");
        while !ops.eof() {
            if let Operator::I32Const { value } = ops.read().expect("op") {
                constants.push(value as u32);
            }
        }
        functions.push(constants);
    }
    functions
}

/// The `i32.const` immediates of the defined function exported as `export_name`.
fn exported_function_constants(wasm: &[u8], export_name: &str) -> Vec<u32> {
    let mut import_count = 0;
    let mut target = None;
    for payload in Parser::new(0).parse_all(wasm) {
        match payload.expect("valid wasm") {
            Payload::ImportSection(reader) => {
                for import in reader {
                    let Ok(wasmparser::Imports::Single(_, import)) = import else {
                        panic!("valid import")
                    };
                    if matches!(import.ty, wasmparser::TypeRef::Func(_)) {
                        import_count += 1;
                    }
                }
            }
            Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export.expect("valid export");
                    if export.name == export_name && export.kind == wasmparser::ExternalKind::Func {
                        target = Some(export.index as usize);
                    }
                }
            }
            _ => {}
        }
    }
    let index = target.unwrap_or_else(|| panic!("export {export_name:?} not found"));
    function_constants(wasm).swap_remove(index - import_count)
}

/// Split modules export nothing; their entry is reached through the function
/// table. Asserts that some function of `wasm` has exactly these constants.
fn assert_some_function_refers_to(wasm: &[u8], constants: &[u32]) {
    let functions = function_constants(wasm);
    assert!(
        functions.contains(&constants.to_vec()),
        "expected a function referring to {constants:?}, got {functions:?}",
    );
}

#[test]
fn shared_data_symbols_are_emitted_once_and_split_data_is_relocated() {
    let _ = tracing_subscriber::fmt::try_init();

    let mut data = SHARED.to_vec();
    data.extend_from_slice(SPLIT_ONLY);
    let input = Input {
        data,
        alignment: 0,
        symbols: vec![
            DataSymbol(
                "shared_tail",
                SHARED_TAIL_OFFSET,
                SHARED.len() - SHARED_TAIL_OFFSET,
            ),
            DataSymbol("shared_full", 0, SHARED.len()),
            DataSymbol("split_only", SHARED.len(), SPLIT_ONLY.len()),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![0]),
            Func(Owner::Split("testsplit"), vec![1, 2]),
        ],
        extra_segments: vec![],
        data_relocs: vec![],
    };
    let output = split(&input);

    // --- Assertion 1: the shared bytes exist once, in the main module.
    assert_eq!(
        single_data_segment(&output.main),
        (SEGMENT_BASE, SHARED.to_vec()),
        "main module should contain exactly the shared bytes at the segment base",
    );
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![SEGMENT_BASE + SHARED_TAIL_OFFSET as u32]
    );

    // --- Assertion 2: the split-only bytes moved into the split module, and
    // its function refers to the main module's copy of the shared bytes.
    let split_module = output.split("testsplit");
    let (split_only_addr, split_only_bytes) = single_data_segment(split_module);
    assert_eq!(split_only_bytes, SPLIT_ONLY);
    assert!(
        split_only_addr >= SEGMENT_BASE + SHARED.len() as u32,
        "split data must not overlap the main module's data",
    );
    assert!(
        split_only_addr + SPLIT_ONLY.len() as u32 <= SEGMENT_BASE + input.data.len() as u32,
        "relocated segment must not grow past its input",
    );
    assert_some_function_refers_to(split_module, &[SEGMENT_BASE, split_only_addr]);
}

#[test]
fn overlapping_symbols_keep_the_strictest_alignment() {
    let _ = tracing_subscriber::fmt::try_init();

    let input = Input {
        // four unreferenced bytes leave room for the parts to be aligned
        data: b"ABCDEFGx....".to_vec(),
        alignment: 2,
        symbols: vec![
            DataSymbol("whole", 0, 7),
            DataSymbol("word", 0, 4),
            DataSymbol("main_byte", 7, 1),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![2]),
            Func(Owner::Split("a"), vec![0, 1]),
        ],
        extra_segments: vec![],
        data_relocs: vec![],
    };
    let output = split(&input);

    let split_a = output.split("a");
    let (a_addr, a_bytes) = single_data_segment(split_a);
    assert_eq!(a_bytes, b"ABCDEFG");
    assert_eq!(
        a_addr % 4,
        0,
        "the word inside the merged range must stay 4-aligned"
    );
    assert_some_function_refers_to(split_a, &[a_addr, a_addr]);

    let (main_addr, main_bytes) = single_data_segment(&output.main);
    assert_eq!(main_bytes, b"x");
    assert!(
        main_addr >= a_addr + 7 || main_addr < a_addr,
        "parts must not overlap"
    );
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![main_addr]
    );
}

#[test]
fn pointer_inside_a_contained_symbol_pulls_its_target_into_main() {
    let _ = tracing_subscriber::fmt::try_init();

    // outer = "ABCD" + pointer; the pointer is the tail that main uses, and it points at target.
    let mut data = b"ABCD".to_vec();
    data.extend_from_slice(&[0; 4]); // pointer, written by the builder
    data.extend_from_slice(&9u32.to_le_bytes());
    let input = Input {
        data: data.clone(),
        alignment: 2,
        symbols: vec![
            DataSymbol("outer", 0, 8),
            DataSymbol("tail", 4, 4),
            DataSymbol("target", 8, 4),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![1]),
            Func(Owner::Split("a"), vec![0]),
        ],
        extra_segments: vec![],
        data_relocs: vec![(4, 2)],
    };
    let output = split(&input);

    // Main reads the pointer, so the target must be there before the split loads: everything
    // ends up in main, with the pointer relocated to the target's (unchanged) address.
    let (main_addr, main_bytes) = single_data_segment(&output.main);
    assert_eq!(main_addr, SEGMENT_BASE);
    let mut expected = data.clone();
    expected[4..8].copy_from_slice(&(SEGMENT_BASE + 8).to_le_bytes());
    assert_eq!(
        main_bytes, expected,
        "main must hold outer, its pointer and the target"
    );
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![SEGMENT_BASE + 4]
    );
    let split_a = output.split("a");
    assert_eq!(
        data_segments(split_a),
        vec![],
        "a shares all its data with main"
    );
    assert_some_function_refers_to(split_a, &[SEGMENT_BASE]);
}

#[test]
fn relocation_crossing_an_inner_symbol_belongs_to_the_containing_symbol() {
    let _ = tracing_subscriber::fmt::try_init();

    // outer = "AB" + pointer + "CD", inner = the last four bytes of outer, so the pointer
    // straddles inner's start. Only outer contains it, so its target follows outer to main.
    let mut data = b"AB".to_vec();
    data.extend_from_slice(&[0; 4]); // pointer, written by the builder
    data.extend_from_slice(b"CD");
    data.extend_from_slice(&9u32.to_le_bytes()); // target
    let input = Input {
        data: data.clone(),
        alignment: 2,
        symbols: vec![
            DataSymbol("outer", 0, 8),
            DataSymbol("inner", 4, 4),
            DataSymbol("target", 8, 4),
        ],
        extra_segments: vec![],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![0]),
            Func(Owner::Split("a"), vec![1]),
        ],
        data_relocs: vec![(2, 2)],
    };
    let output = split(&input);

    let (main_addr, main_bytes) = single_data_segment(&output.main);
    assert_eq!(main_addr, SEGMENT_BASE);
    let mut expected = data.clone();
    expected[2..6].copy_from_slice(&(SEGMENT_BASE + 8).to_le_bytes());
    assert_eq!(
        main_bytes, expected,
        "main must hold outer, its pointer and the target"
    );
    assert_eq!(data_segments(output.split("a")), vec![]);
    assert_some_function_refers_to(output.split("a"), &[SEGMENT_BASE + 4]);
}

#[test]
fn data_shared_between_splits_is_emitted_from_their_chunk() {
    let _ = tracing_subscriber::fmt::try_init();

    let mut data = SHARED.to_vec();
    data.extend_from_slice(SPLIT_ONLY);
    data.extend_from_slice(MAIN_ONLY);
    let main_only_offset = SHARED.len() + SPLIT_ONLY.len();
    let input = Input {
        data,
        alignment: 0,
        symbols: vec![
            DataSymbol("shared_full", 0, SHARED.len()),
            DataSymbol(
                "shared_tail",
                SHARED_TAIL_OFFSET,
                SHARED.len() - SHARED_TAIL_OFFSET,
            ),
            DataSymbol("split_only", SHARED.len(), SPLIT_ONLY.len()),
            DataSymbol("main_only", main_only_offset, MAIN_ONLY.len()),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![3]),
            // `a` uses the whole string, `a` and `b` its tail: the tail is shared by both
            // splits and lives in their chunk. The whole string overlaps it and must follow.
            Func(Owner::Split("a"), vec![0, 1]),
            Func(Owner::Split("b"), vec![1, 2]),
        ],
        extra_segments: vec![],
        data_relocs: vec![],
    };
    let output = split(&input);
    let segment_end = SEGMENT_BASE + input.data.len() as u32;

    // main only holds its own bytes
    assert_eq!(
        single_data_segment(&output.main),
        (SEGMENT_BASE, MAIN_ONLY.to_vec()),
        "main module should contain exactly its own bytes",
    );
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![SEGMENT_BASE]
    );

    // the whole shared string is in the one chunk shared by `a` and `b`
    let [chunk] = output.chunks.as_slice() else {
        panic!("expected exactly one chunk, got {}", output.chunks.len());
    };
    let (shared_addr, shared_bytes) = single_data_segment(chunk);
    assert_eq!(shared_bytes, SHARED);
    let shared_tail_addr = shared_addr + SHARED_TAIL_OFFSET as u32;

    // `a` has no data of its own left, `b` keeps its own bytes
    let split_a = output.split("a");
    assert_eq!(data_segments(split_a), vec![], "all of a's data is shared");
    assert_some_function_refers_to(split_a, &[shared_addr, shared_tail_addr]);

    let split_b = output.split("b");
    let (split_only_addr, split_only_bytes) = single_data_segment(split_b);
    assert_eq!(split_only_bytes, SPLIT_ONLY);
    assert_some_function_refers_to(split_b, &[shared_tail_addr, split_only_addr]);

    // everything stays inside the input segment, without overlaps
    let mut regions = [
        (SEGMENT_BASE, MAIN_ONLY.len() as u32),
        (shared_addr, SHARED.len() as u32),
        (split_only_addr, SPLIT_ONLY.len() as u32),
    ];
    regions.sort();
    for window in regions.windows(2) {
        let [(start, len), (next, _)] = window else {
            unreachable!()
        };
        assert!(start + len <= *next, "regions overlap: {regions:?}");
    }
    let (last, len) = regions[2];
    assert!(last + len <= segment_end, "segment grew past its input");
}

#[test]
fn chunk_placement_uses_the_exact_chunk_of_all_requiring_splits() {
    let _ = tracing_subscriber::fmt::try_init();

    // `outer` (split a) contains `i1` (also b), `i2` (also c) and `i3` (also b, c, e). The
    // range is therefore required by {a,b}, then {a,b,c}, then {a,b,c,e}. Only the first and
    // the last set have a chunk; {a,b,c} does not, but its superset {a,b,c,d} (holding `w`)
    // does. Choosing that superset for the intermediate set must not send the range to main
    // once `i3` is merged.
    let data = b"iiiijjjjkkkkWWM.".to_vec();
    let input = Input {
        data: data.clone(),
        alignment: 0,
        symbols: vec![
            DataSymbol("outer", 0, 12),
            DataSymbol("i1", 0, 4),
            DataSymbol("i2", 4, 4),
            DataSymbol("i3", 8, 4),
            DataSymbol("w", 12, 2),
            DataSymbol("m", 14, 1),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![5]),
            Func(Owner::Split("a"), vec![0, 4]),
            Func(Owner::Split("b"), vec![1, 3, 4]),
            Func(Owner::Split("c"), vec![2, 3, 4]),
            Func(Owner::Split("d"), vec![4]),
            Func(Owner::Split("e"), vec![3]),
        ],
        extra_segments: vec![],
        data_relocs: vec![],
    };
    let output = split(&input);

    assert_eq!(
        single_data_segment(&output.main),
        (SEGMENT_BASE, b"M".to_vec()),
        "main should only hold its own byte",
    );
    // the data of each chunk; chunks whose symbols all moved elsewhere have none
    let chunk_data: Vec<Vec<Vec<u8>>> = output
        .chunks
        .iter()
        .map(|chunk| {
            data_segments(chunk)
                .into_iter()
                .map(|(_, bytes)| bytes)
                .collect()
        })
        .collect();
    assert!(
        chunk_data.contains(&vec![b"iiiijjjjkkkk".to_vec()]),
        "the shared range should be in a chunk of its own, got {chunk_data:?}",
    );
    assert!(
        chunk_data.contains(&vec![b"WW".to_vec()]),
        "w should be in the {{a,b,c,d}} chunk on its own, got {chunk_data:?}",
    );
    for split in ["a", "b", "c", "d", "e"] {
        assert_eq!(
            data_segments(output.split(split)),
            vec![],
            "{split} shares all its data"
        );
    }
}

#[test]
fn chunk_placement_records_every_requiring_split() {
    let _ = tracing_subscriber::fmt::try_init();

    // `outer` (split a) contains `i1` (also b), `i2` (also c), `i3` (also b, c, d) and `i4`
    // (also b, c, e). After `i2` the range is required by {a,b,c}, for which the superset chunk
    // {a,b,c,d} (holding `i3`) is chosen. `i3`'s owner is that very chunk, so `d` must still be
    // recorded: after `i4` the range is required by {a,b,c,d,e}, which only main satisfies.
    let data = b"iiiijjjjkkkkllllM".to_vec();
    let input = Input {
        data: data.clone(),
        alignment: 0,
        symbols: vec![
            DataSymbol("outer", 0, 16),
            DataSymbol("i1", 0, 4),
            DataSymbol("i2", 4, 4),
            DataSymbol("i3", 8, 4),
            DataSymbol("i4", 12, 4),
            DataSymbol("m", 16, 1),
        ],
        extra_segments: vec![],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![5]),
            Func(Owner::Split("a"), vec![0]),
            Func(Owner::Split("b"), vec![1, 3, 4]),
            Func(Owner::Split("c"), vec![2, 3, 4]),
            Func(Owner::Split("d"), vec![3]),
            Func(Owner::Split("e"), vec![4]),
        ],
        data_relocs: vec![],
    };
    let output = split(&input);

    assert_eq!(
        single_data_segment(&output.main),
        (SEGMENT_BASE, data),
        "the range is needed by every split, so only main can hold it",
    );
    for chunk in &output.chunks {
        assert_eq!(data_segments(chunk), vec![], "no chunk may hold the range");
    }
    for split in ["a", "b", "c", "d", "e"] {
        assert_eq!(data_segments(output.split(split)), vec![]);
    }
}

#[test]
fn mixed_alignments_pack_without_padding_across_modules() {
    let _ = tracing_subscriber::fmt::try_init();

    let data = b"Mab.AAAACCCCBBBB.".to_vec();
    let input = Input {
        data: data.clone(),
        alignment: 2,
        symbols: vec![
            DataSymbol("main_byte", 0, 1),
            DataSymbol("a_byte", 1, 1),
            DataSymbol("a_word1", 4, 4),
            DataSymbol("a_word2", 8, 4),
            DataSymbol("b_byte", 2, 1),
            DataSymbol("b_word", 12, 4),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![0]),
            Func(Owner::Split("a"), vec![1, 2, 3]),
            Func(Owner::Split("b"), vec![4, 5]),
        ],
        extra_segments: vec![],
        data_relocs: vec![],
    };
    let output = split(&input);

    // All words come first, then all bytes, so nothing needs padding: 15 of the 17 input bytes
    // are used, in module order within each alignment.
    let split_a = output.split("a");
    let split_b = output.split("b");
    assert_eq!(
        data_segments(split_a),
        vec![
            (SEGMENT_BASE, b"AAAACCCC".to_vec()),
            (SEGMENT_BASE + 13, b"a".to_vec())
        ]
    );
    assert_eq!(
        data_segments(split_b),
        vec![
            (SEGMENT_BASE + 8, b"BBBB".to_vec()),
            (SEGMENT_BASE + 14, b"b".to_vec())
        ]
    );
    assert_eq!(
        data_segments(&output.main),
        vec![(SEGMENT_BASE + 12, b"M".to_vec())]
    );
    assert_some_function_refers_to(
        split_a,
        &[SEGMENT_BASE + 13, SEGMENT_BASE, SEGMENT_BASE + 4],
    );
    assert_some_function_refers_to(split_b, &[SEGMENT_BASE + 14, SEGMENT_BASE + 8]);
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![SEGMENT_BASE + 12]
    );

    // The input's only segment keeps index 0 in every module, holding the module's first
    // fragment; further fragments are appended, and the data count matches.
    for (name, module, first, extra) in [
        ("a", split_a, Some(SEGMENT_BASE), vec![SEGMENT_BASE + 13]),
        (
            "b",
            split_b,
            Some(SEGMENT_BASE + 8),
            vec![SEGMENT_BASE + 14],
        ),
        (
            "main",
            output.main.as_slice(),
            Some(SEGMENT_BASE + 12),
            vec![],
        ),
    ] {
        let (segments, count) = all_data_segments(module);
        assert_eq!(segments.len() as u32, count, "{name}: data count");
        assert_eq!(
            segments.len(),
            1 + extra.len(),
            "{name}: appended fragments"
        );
        assert_eq!(
            segments[0].as_ref().map(|(addr, _)| *addr),
            first,
            "{name}: slot of the input segment"
        );
        let appended: Vec<u32> = segments[1..]
            .iter()
            .map(|segment| {
                segment
                    .as_ref()
                    .expect("appended fragments are not empty")
                    .0
            })
            .collect();
        assert_eq!(appended, extra, "{name}: appended fragment addresses");
    }
}

#[test]
fn overlong_segment_folds_the_smallest_split_into_main() {
    let _ = tracing_subscriber::fmt::try_init();

    // `whole` (split a) starts at offset 1 and contains a 4-aligned word, so its merged range
    // must be placed at an offset congruent to 1 modulo 4. Bytes 9..12 are unreferenced.
    let data = b"MABCDEFGH...BBBB".to_vec();
    let input = Input {
        data: data.clone(),
        alignment: 2,
        symbols: vec![
            DataSymbol("main_byte", 0, 1),
            DataSymbol("whole", 1, 8),
            DataSymbol("word", 4, 4),
            DataSymbol("b_word", 12, 4),
        ],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![0]),
            Func(Owner::Split("a"), vec![1, 2]),
            Func(Owner::Split("b"), vec![3]),
        ],
        extra_segments: vec![],
        data_relocs: vec![],
    };
    let output = split(&input);

    // Without folding: a's range at 1..9, b's word at 12..16, main's byte at 16: one byte too
    // long. Folding `b`, the smaller split, into main puts its word first at 0..4, then a's
    // range at 5..13 and main's byte at 13.
    assert_eq!(
        data_segments(&output.main),
        vec![
            (SEGMENT_BASE, b"BBBB".to_vec()),
            (SEGMENT_BASE + 13, b"M".to_vec())
        ],
        "main should hold b's word and its own byte",
    );
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![SEGMENT_BASE + 13]
    );
    let split_b = output.split("b");
    assert_eq!(data_segments(split_b), vec![], "b's data moved into main");
    assert_some_function_refers_to(split_b, &[SEGMENT_BASE]);

    let split_a = output.split("a");
    let (a_addr, a_bytes) = single_data_segment(split_a);
    assert_eq!(a_bytes, b"ABCDEFGH");
    assert_eq!(
        (a_addr + 3) % 4,
        0,
        "the word inside a's range must stay 4-aligned"
    );
    assert!(a_addr >= SEGMENT_BASE + 4 && a_addr + 8 <= SEGMENT_BASE + 13);
    assert_some_function_refers_to(split_a, &[a_addr, a_addr + 3]);
}

#[test]
fn overlapping_input_segments_keep_their_order() {
    let _ = tracing_subscriber::fmt::try_init();

    // Segment 1 at SEGMENT_BASE + 8 overlaps the last byte of segment 0 and is initialized
    // after it, so that byte must end up as "X". Relocating segment 0 would emit main's "m"
    // in a segment appended after segment 1.
    let input = Input {
        data: b"AAAABBBBm".to_vec(),
        alignment: 2,
        symbols: vec![
            DataSymbol("main_word", 0, 4),
            DataSymbol("split_word", 4, 4),
            DataSymbol("main_byte", 8, 1),
        ],
        extra_segments: vec![(SEGMENT_BASE + 8, b"X".to_vec(), vec![DataSymbol("x", 0, 1)])],
        funcs: vec![
            Func(Owner::Main("main_reads"), vec![0, 2, 3]),
            Func(Owner::Split("a"), vec![1]),
        ],
        data_relocs: vec![],
    };
    let output = split(&input);

    assert_eq!(
        data_segments(&output.main),
        vec![
            (SEGMENT_BASE, b"AAAABBBBm".to_vec()),
            (SEGMENT_BASE + 8, b"X".to_vec())
        ],
        "both segments stay as they are, in input order",
    );
    assert_eq!(data_segments(output.split("a")), vec![]);
    assert_some_function_refers_to(output.split("a"), &[SEGMENT_BASE + 4]);
    assert_eq!(
        exported_function_constants(&output.main, "main_reads"),
        vec![SEGMENT_BASE, SEGMENT_BASE + 8, SEGMENT_BASE + 8]
    );
}
