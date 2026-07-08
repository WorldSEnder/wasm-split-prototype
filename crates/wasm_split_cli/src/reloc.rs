use std::{
    cell::OnceCell,
    collections::{HashMap, HashSet},
    ops::Range,
};

use eyre::{anyhow, bail, ensure, Result};
use tracing::trace;
use wasmparser::{
    CustomSectionReader, Data, DefinedDataSymbol, ElementItems, ElementKind, Export, ExternalKind,
    KnownCustom, Linking, Payload, RelocAddendKind, RelocationEntry, RelocationType, Segment,
    SymbolFlags, SymbolInfo,
};

use crate::{
    magic_constants,
    read::{GlobalId, InputFuncId, InputModule, InputOffset, SectionId, TableId, TagId},
    util::{find_subrange, shift_range},
};

// An offset (index) into the bytes of the input module
pub type SectionIndex = SectionId;
pub type SymbolIndex = usize;

#[derive(Default)]
pub struct RelocInfoParser<'a> {
    info: RelocInfo<'a>,
    has_linking_section: bool,
    // We NEED this to be present to identify the table to fix-up
    indirect_function_table: Option<TableId>,
}

impl<'a> RelocInfoParser<'a> {
    fn visit_linking(&mut self, subsection: Linking<'a>) -> Result<()> {
        match subsection {
            Linking::SegmentInfo(segments) => {
                ensure!(self.info.segments.is_empty(), "duplicate segments info");
                self.info.segments = segments.into_iter().collect::<Result<_, _>>()?;
                return Ok(());
            }
            Linking::SymbolTable(map) => {
                ensure!(self.info.symbols.is_empty(), "duplicate symbol table");
                self.info.symbols = map.into_iter().collect::<Result<Vec<_>, _>>()?;
                for sym in &self.info.symbols {
                    #[allow(clippy::single_match)] // other special cases might follow
                    match *sym {
                        SymbolInfo::Table {
                            name: Some("__indirect_function_table"),
                            index,
                            ..
                        } => {
                            self.indirect_function_table = Some(index as TableId);
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }
    fn visit_custom(&mut self, custom: &CustomSectionReader<'a>) -> Result<bool> {
        match custom.as_known() {
            KnownCustom::Linking(reader) => {
                self.has_linking_section = true;
                for subsection in reader.subsections() {
                    let () = self.visit_linking(subsection?)?;
                }
                Ok(true)
            }
            KnownCustom::Reloc(reader) => {
                let mut reloc_entries = reader
                    .entries()
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()?;
                // We need to slices of entries when we search for a specific offset
                // We *might* be fine with assuming that the entries are already sorted, but a single pass to correct this
                // doesn't cost a lot of performance.
                reloc_entries.sort_unstable_by_key(|entry| entry.offset);
                self.info
                    .relocs
                    .insert(reader.section_index() as SectionIndex, reloc_entries);
                Ok(true)
            }
            _ => Ok(false),
        }
    }
    pub fn visit_payload(&mut self, payload: &Payload<'a>) -> Result<bool> {
        let section_index = self.info.relocatable_ranges.len();
        if let Some((_, mut section_range)) = payload.as_section() {
            if let Payload::CustomSection(custom) = payload {
                // Not sure where this is specified, and it might even be wrong.
                // https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md#relocation-sections says:
                // > offset: relative to the relevant section's contents: offset zero is immediately after the id and size of the section
                // well, the data offset comes after the name of the custom section, not "immediately after the id and size".
                // alas, llvm seems to generate relocations relative to data offset though.
                section_range.start = custom.data_offset();
            }
            self.info.relocatable_ranges.push(section_range);
        }
        match payload {
            Payload::DataSection(_) => {
                self.info.data_section_index = section_index;
                Ok(true)
            }
            Payload::CodeSectionStart { .. } => {
                self.info.code_section_index = section_index;
                Ok(true)
            }
            Payload::CustomSection(reader) => {
                self.info.custom_sections.insert(section_index);
                self.visit_custom(reader)
            }
            _ => Ok(false),
        }
    }
    pub fn finish(self, module: &InputModule<'a>) -> Result<RelocInfo<'a>> {
        let mut info = self.info;
        info.data_symbols = get_data_symbols(&module.data_segments, &info.symbols)?;
        if !self.has_linking_section {
            bail!("No linking section found. Make sure that your program is compiled with `-Clink-args=--emit-relocs`.");
        }
        let Some(indirect_function_table) = self.indirect_function_table else {
            bail!("No indirect function table found in the reloc data");
        };
        info.indirect_table = indirect_function_table;
        get_indirect_functions(&mut info, indirect_function_table, module)?;
        reconstruct_global_symbols(&mut info, module)?;
        Ok(info)
    }
}

fn get_indirect_functions(
    this: &mut RelocInfo<'_>,
    iftable: TableId,
    module: &InputModule,
) -> Result<()> {
    let mut input_indirect_funcs = HashSet::new();
    for elems in &module.elements {
        let ElementKind::Active {
            table_index,
            offset_expr: _,
        } = elems.kind
        else {
            continue;
        };
        if table_index.unwrap_or(0) as usize != iftable {
            continue;
        }
        let ElementItems::Functions(funcs) = &elems.items else {
            bail!("expected immediate function ids in the indirect function table");
        };
        let funcs: Vec<u32> = funcs.clone().into_iter().collect::<Result<Vec<_>, _>>()?;
        input_indirect_funcs.extend(funcs.into_iter().map(|f| f as usize));
    }

    let mut visible_functions = HashSet::new();
    for symbol in &this.symbols {
        let SymbolInfo::Func { index, flags, .. } = *symbol else {
            continue;
        };
        if !input_indirect_funcs.contains(&(index as usize)) {
            continue;
        }
        let mut keep = flags.contains(SymbolFlags::NO_STRIP);
        keep |= flags.contains(SymbolFlags::EXPORTED | SymbolFlags::BINDING_WEAK);
        if !keep {
            continue;
        }
        visible_functions.insert(index as InputFuncId);
    }

    let mut referenced_indirects = visible_functions.clone();
    for relocation in this.relocs.values().flat_map(|relocs| relocs.iter()) {
        use RelocationType::*;
        if !matches!(
            relocation.ty,
            TableIndexI32
                | TableIndexI64
                | TableIndexSleb
                | TableIndexSleb64
                | TableIndexRelSleb
                | TableIndexRelSleb64
        ) {
            continue;
        }
        let symbol = &this.symbols[relocation.index as usize];
        let SymbolInfo::Func { index, .. } = *symbol else {
            bail!("invalid TABLE_INDEX relocation expected");
        };
        referenced_indirects.insert(index as InputFuncId);
    }

    this.visible_indirects = visible_functions;
    this.referenced_indirects = referenced_indirects;
    Ok(())
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct DataSymbol {
    pub symbol_index: SymbolIndex,
    // Range relative to the start of the WebAssembly file.
    pub range: Range<InputOffset>,
}

fn get_data_symbols(data_segments: &[Data], symbols: &[SymbolInfo]) -> Result<Vec<DataSymbol>> {
    let mut data_symbols = Vec::new();
    for (symbol_index, info) in symbols.iter().enumerate() {
        let SymbolInfo::Data {
            symbol: Some(symbol),
            ..
        } = info
        else {
            continue;
        };
        if symbol.size == 0 {
            // Ignore zero-size symbols since they cannot be the target of a relocation.
            continue;
        }
        let data_segment = data_segments
            .get(symbol.index as usize)
            .ok_or_else(|| anyhow!("Invalid data segment index in symbol: {:?}", symbol))?;
        let symbol_range = shift_range(0..symbol.size as usize, symbol.offset as usize);
        if symbol_range.end > data_segment.data.len() {
            bail!(
                "Invalid symbol {symbol:?} for data segment of size {:?}",
                data_segment.data.len()
            );
        }
        let data_offset = data_segment.range.end - data_segment.data.len();
        data_symbols.push(DataSymbol {
            symbol_index,
            range: shift_range(symbol_range, data_offset),
        });
    }
    // We assume that these are sorted by range start later on
    data_symbols.sort_unstable_by_key(|symbol| symbol.range.start);
    Ok(data_symbols)
}

fn reconstruct_global_symbols(reloc_info: &mut RelocInfo<'_>, module: &InputModule) -> Result<()> {
    let symbol_as_global = &mut reloc_info.symbol_as_global;
    debug_assert!(
        symbol_as_global.is_empty(),
        "should not yet contain reconstructed data"
    );
    for &export in &module.exports {
        let Export {
            kind: ExternalKind::Global,
            index: global_index,
            name: global_name,
        } = export
        else {
            continue;
        };
        // You would think that there is reloc data available that tells us which symbols are exported
        // but there is not. The flags also do not tell us this information (which might be a bug in the
        // way symbol information is emitted). In any case, we reconstruct this information.
        let Some((symbol_index, symbol)) =
            reloc_info
                .symbols
                .iter()
                .enumerate()
                .find(|(_, &symbol_info)| {
                    let SymbolInfo::Data {
                        flags: _,
                        name: symbol_name,
                        symbol: _,
                    } = symbol_info
                    else {
                        return false;
                    };
                    symbol_name == global_name
                })
        else {
            continue;
        };
        let global_index: GlobalId = global_index.try_into().unwrap();
        if global_name.starts_with(magic_constants::GLOBAL_WASM_SPLIT_MARKER) {
            // let's try to be conservative and double check that the marker symbol is zero-sized data
            match *symbol {
                SymbolInfo::Data {
                    symbol: Some(symbol),
                    ..
                } if symbol.size == 0 => {
                    reloc_info.split_marker_globals.insert(global_index);
                    continue;
                }
                SymbolInfo::Data { .. } => tracing::warn!("expected zero sized marker export"),
                _ => tracing::warn!("expected wasm split marker export to export a data symbol"),
            }
        }
        tracing::trace!(
            "Recovered global symbol {global_index} mapping to {symbol_index} [{symbol:?}]"
        );
        #[cfg(debug_assertions)]
        {
            use eyre::ensure;
            use eyre::Context;
            use wasmparser::{Operator, ValType};

            let global = &module.globals[global_index];
            ensure!(
                matches!(global.ty.content_type, ValType::I32 | ValType::I64),
                "expected be a memory address"
            );
            ensure!(
                !global.ty.mutable && !global.ty.shared,
                "a memory address should be immutable and not shared"
            );
            let _addr: usize = match global.init_expr.get_operators_reader().read() {
                Ok(Operator::I32Const { value }) => {
                    usize::try_from(value).context("a valid address should fit into a usize")?
                }
                Ok(Operator::I64Const { value }) => {
                    usize::try_from(value).context("a valid address should fit into a usize")?
                }
                _ => {
                    bail!("expected a constant initializer expression for an exported data symbol")
                }
            };
            // could decode the symbol's address from the data-segment base address here too. Trust that this is correct
        }
        symbol_as_global.insert(global_index, symbol_index);
    }
    Ok(())
}

#[derive(Default)]
pub struct RelocInfo<'a> {
    // The relocatable range within each section. The start offset is the base from which
    // the relocation entry is offset from.
    pub relocatable_ranges: Vec<Range<InputOffset>>,
    pub segments: Vec<Segment<'a>>,
    pub custom_sections: HashSet<SectionId>,
    invalid_reloc_warn: OnceCell<()>,

    pub code_section_index: SectionIndex,
    pub data_section_index: SectionIndex,
    pub data_symbols: Vec<DataSymbol>,
    pub symbols: Vec<SymbolInfo<'a>>,
    pub relocs: HashMap<usize, Vec<RelocationEntry>>,

    pub indirect_table: TableId,
    pub stack_pointer: GlobalId,
    pub visible_indirects: HashSet<InputFuncId>,
    pub referenced_indirects: HashSet<InputFuncId>,
    // `#i -> #s` if Global #i contains the address of symbol #s
    pub symbol_as_global: HashMap<GlobalId, SymbolIndex>,
    pub split_marker_globals: HashSet<GlobalId>,
}

impl RelocInfo<'_> {
    pub fn print_relocs(&self) {
        use wasmparser::RelocAddendKind;
        if !tracing::event_enabled!(tracing::Level::TRACE) {
            return;
        }

        trace!("Symbols >>>>>>>>>>>>>>>>>>>>>>>>");
        for symbol in &self.symbols {
            trace!("{symbol:?}");
        }
        trace!("Symbols <<<<<<<<<<<<<<<<<<<<<<<<");
        for (section, relocs) in &self.relocs {
            trace!(%section, "Reloc section >>>>>>>>>>>>>>>>>>>>>>>>");
            for reloc in relocs {
                struct InvalidRelocIndex; // TODO: replace with std::fmt::from_fn
                impl std::fmt::Debug for InvalidRelocIndex {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(f, "<invalid reloc index>")
                    }
                }
                let symbol = self
                    .symbols
                    .get(reloc.index as usize)
                    .map(|r| r as &dyn std::fmt::Debug)
                    .unwrap_or(&InvalidRelocIndex);
                let mut symbol_info = format!("  [{}] = {:?}({symbol:?})", reloc.offset, reloc.ty);
                if matches!(
                    reloc.ty.addend_kind(),
                    RelocAddendKind::Addend64 | RelocAddendKind::Addend32
                ) {
                    symbol_info += &format!(" {:+}", reloc.addend);
                }
                trace!(%symbol_info);
            }
            trace!(%section, "Reloc section <<<<<<<<<<<<<<<<<<<<<<<<");
        }
    }
    pub fn reloc_base(&self, section: SectionIndex) -> InputOffset {
        self.relocatable_ranges[section].start
    }
    pub fn data_section_reloc_base(&self) -> InputOffset {
        self.reloc_base(self.data_section_index)
    }
    pub fn code_section_reloc_base(&self) -> InputOffset {
        self.reloc_base(self.code_section_index)
    }
    pub fn iter_section_relocs(&self, section: SectionIndex) -> &'_ [RelocationEntry] {
        self.relocs
            .get(&section)
            .map(|relocs| &relocs[..])
            .unwrap_or_default()
    }
    pub fn iter_data_relocs(&self) -> impl Iterator<Item = &'_ RelocationEntry> {
        self.iter_section_relocs(self.data_section_index).iter()
    }
    pub fn iter_code_relocs(&self) -> impl Iterator<Item = &'_ RelocationEntry> {
        self.iter_section_relocs(self.code_section_index).iter()
    }
    pub fn get_relocations_for_range(
        &self,
        range: &Range<InputOffset>,
    ) -> (
        InputOffset,
        impl Iterator<Item = &RelocationEntry> + use<'_>,
    ) {
        let target_sections = find_subrange(
            &self.relocatable_ranges,
            |section_range| section_range.end >= range.end,
            |section_range| section_range.start < range.end,
        );
        let section = target_sections.start;
        let section_range = &self.relocatable_ranges[section];
        let reloc_base = section_range.start;
        assert!(
            section_range.start <= range.start && range.end <= section_range.end,
            "range to rellocate should be fully contained in one section"
        );
        let section_subrange = (range.start - reloc_base)..(range.end - reloc_base);

        let section_relocs = self.iter_section_relocs(section);
        let reloc_range = find_subrange(
            section_relocs,
            |reloc| (reloc.offset as usize) >= section_subrange.start,
            |reloc| (reloc.offset as usize) < section_subrange.end,
        );

        (reloc_base, section_relocs[reloc_range].iter())
    }

    pub fn get_relocated_data(
        module: &InputModule,
        range: Range<InputOffset>,
        target: &impl RelocTarget,
    ) -> Result<Vec<u8>> {
        let this = &module.reloc_info;
        let mut data = Vec::from(&module.raw[range.clone()]);
        let (reloc_base, relocs) = this.get_relocations_for_range(&range);
        let reloc_base_to_data_off = range.start - reloc_base;
        for relocation in relocs {
            this.apply_relocation(target, &mut data, reloc_base_to_data_off, relocation)?;
        }
        Ok(data)
    }

    pub fn expand_relocation(&self, relocation: &RelocationEntry) -> Result<RelocDetails<'_>> {
        use wasmparser::RelocationType::*;
        let symbol_index = relocation.index as usize;
        if symbol_index > self.symbols.len() {
            bail!("Found {relocation:?} with invalid symbol index?");
        }
        let symbol = &self.symbols[symbol_index];
        let ty = relocation.ty;
        match ty {
            MemoryAddrLeb | MemoryAddrSleb | MemoryAddrI32 | MemoryAddrLeb64 | MemoryAddrSleb64
            | MemoryAddrI64 | MemoryAddrTlsSleb | MemoryAddrLocrelI32 | MemoryAddrTlsSleb64
            | MemoryAddrRelSleb | MemoryAddrRelSleb64 => {
                let wasmparser::SymbolInfo::Data {
                    flags,
                    name,
                    symbol: ref symbol_def,
                } = *symbol
                else {
                    bail!("Expected a data symbol as target of a MEMORY_ADDR relocation, got {symbol:?}");
                };
                Ok(RelocDetails::MemoryAddr(DataDetails {
                    symbol_index,
                    _flags: flags,
                    _name: name,
                    definition: symbol_def.as_ref(),
                }))
            }
            TableIndexSleb | TableIndexSleb64 | TableIndexI32 | TableIndexI64 => {
                let wasmparser::SymbolInfo::Func { flags, index, name } = *symbol else {
                    bail!("Expected a func symbol as target of a TABLE_INDEX relocation, got {symbol:?}");
                };
                Ok(RelocDetails::TableIndex(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as InputFuncId,
                    _name: name,
                }))
            }
            TableIndexRelSleb | TableIndexRelSleb64 => {
                let wasmparser::SymbolInfo::Func { flags, index, name } = *symbol else {
                    bail!("Expected a func symbol as target of a TABLE_INDEX relocation, got {symbol:?}");
                };
                Ok(RelocDetails::RelTableIndex(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as InputFuncId,
                    _name: name,
                }))
            }
            wasmparser::RelocationType::FunctionIndexLeb
            | wasmparser::RelocationType::FunctionIndexI32 => {
                let wasmparser::SymbolInfo::Func { flags, index, name } = *symbol else {
                    bail!("Expected a func symbol as target of a FUNCTION_INDEX relocation, got {symbol:?}");
                };
                Ok(RelocDetails::FunctionIndex(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as InputFuncId,
                    _name: name,
                }))
            }
            wasmparser::RelocationType::TableNumberLeb => {
                let wasmparser::SymbolInfo::Table { flags, index, name } = *symbol else {
                    bail!("Expected a table symbol as target of a TABLE_NUMBER relocation, got {symbol:?}");
                };
                Ok(RelocDetails::TableNumber(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as TableId,
                    _name: name,
                }))
            }
            wasmparser::RelocationType::GlobalIndexI32
            | wasmparser::RelocationType::GlobalIndexLeb => {
                let wasmparser::SymbolInfo::Global { flags, index, name } = *symbol else {
                    bail!("Expected a global symbol as target of a GLOBAL_INDEX relocation, got {symbol:?}");
                };
                Ok(RelocDetails::GlobalIndex(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as GlobalId,
                    _name: name,
                }))
            }
            wasmparser::RelocationType::EventIndexLeb => {
                let wasmparser::SymbolInfo::Event { flags, index, name } = *symbol else {
                    bail!("Expected a global symbol as target of a EVENT_INDEX relocation, got {symbol:?}");
                };
                Ok(RelocDetails::TagIndex(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as TagId,
                    _name: name,
                }))
            }
            wasmparser::RelocationType::TypeIndexLeb => Ok(RelocDetails::TypeIndex {
                _symbol_idx: symbol_index,
                _symbol: *symbol,
            }),
            wasmparser::RelocationType::FunctionOffsetI32
            | wasmparser::RelocationType::FunctionOffsetI64 => {
                let wasmparser::SymbolInfo::Func { flags, index, name } = *symbol else {
                    bail!("Expected a func symbol as target of a FUNCTION_OFFSET relocation, got {symbol:?}");
                };
                Ok(RelocDetails::FunctionOffset(SymbolDetails {
                    _symbol_index: symbol_index,
                    _flags: flags,
                    index: index as InputFuncId,
                    _name: name,
                }))
            }
            wasmparser::RelocationType::SectionOffsetI32 => {
                let wasmparser::SymbolInfo::Section { flags, section } = *symbol else {
                    bail!("Expected a section symbol as target of a SECTION_OFFSET relocation, got {symbol:?}");
                };
                let section = section as SectionId;
                // If the reloc points to a section with semantic context (such as data/code), we expect to relocate against a symbol,
                // not by offset. It is used e.g. to point to DIE in debug_info sections. Why this happens via offset and not by some
                // kind of "symbol" "inside" that custom section with an associated range similar to data symbols is a question I can
                // not answer.
                // NOTE: it seems newer compiler versions do not bother to emit this in any case.
                ensure!(
                    self.custom_sections.contains(&section),
                    "Expect a SECTION_OFFSET relocation to point into a custom section"
                );
                Ok(RelocDetails::SectionOffset(SymbolDetails {
                    _symbol_index: symbol_index,
                    index: section,
                    _flags: flags,
                    _name: None,
                }))
            } // [relocate data segments]
              // TODO: there is no relocation for data segment indices. As such, we'd have to parse the opcodes to find
              // references to passive and declarative data segments. The solution: only handling active segments
              // and don't change the index of passive ones.
        }
    }

    fn apply_relocation<T: RelocTarget>(
        &self,
        reloc_target: &T,
        data: &mut [u8],
        reloc_base_to_data_off: usize,
        relocation: &RelocationEntry,
    ) -> Result<()> {
        let fixup;
        // TODO(MSRV): -1i32.cast_unsigned() since rust 1.87
        let relocation = if relocation.index == (-1i32 as u32) {
            fixup = reloc_target.fixup_reloc_entry(relocation)?;
            &fixup
        } else {
            relocation
        };
        let relocated = if relocation.index == (-1i32 as u32) {
            // We have found a relocation against a symbol that isn't in the symbol table.
            // This most likely means that we will miss out on correct relocations.
            // We must handle this though, as some compilers will emit relocations in the debug section
            // against non-public symbols and use index -1 for those.
            ensure!(
                T::SENTINEL_UNDEF,
                "Invalid relocation {relocation:?} with tombstone symbol couldn't be fixed"
            );
            self.invalid_reloc_warn.get_or_init(|| {
                tracing::warn!(
                    "Skipping relocation {relocation:?} with tombstone symbol (others omitted)"
                );
            });
            // Since we can't ensure that the relocation target can survive unchanged, relocate
            // to a tombstone address.
            Some(SENTINEL_UNDEF)
        } else {
            let details = self.expand_relocation(relocation)?;
            reloc_target.reloc_value(details)?
        };
        let relocation_range = relocation.relocation_range()?;
        let target = &mut data[(relocation_range.start - reloc_base_to_data_off)
            ..(relocation_range.end - reloc_base_to_data_off)];
        let ty = relocation.ty;
        let Some(value) = relocated else {
            return Ok(());
        };
        debug_assert!(
            relocation.addend == 0 || ty.addend_kind() != RelocAddendKind::None,
            "relocation {relocation:?} without addend should have addend == 0, not {}",
            relocation.addend,
        );
        let () = encode_for_ty(
            ty,
            value,
            relocation.addend as isize,
            target,
            T::SENTINEL_UNDEF,
        )?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct SymbolDetails<'a, Idx> {
    pub _symbol_index: usize,
    pub index: Idx,
    pub _flags: SymbolFlags,
    pub _name: Option<&'a str>,
}

#[derive(Debug)]
pub struct DataDetails<'a> {
    pub symbol_index: usize,
    pub _flags: SymbolFlags,
    pub _name: &'a str,
    pub definition: Option<&'a DefinedDataSymbol>,
}

#[derive(Debug)]
pub enum RelocDetails<'a> {
    TypeIndex {
        _symbol_idx: usize,
        _symbol: SymbolInfo<'a>,
    },
    MemoryAddr(DataDetails<'a>),
    TableIndex(SymbolDetails<'a, InputFuncId>),
    RelTableIndex(SymbolDetails<'a, InputFuncId>),
    FunctionIndex(SymbolDetails<'a, InputFuncId>),
    TableNumber(SymbolDetails<'a, TableId>),
    GlobalIndex(SymbolDetails<'a, GlobalId>),
    TagIndex(SymbolDetails<'a, TagId>),
    FunctionOffset(SymbolDetails<'a, InputFuncId>),
    SectionOffset(SymbolDetails<'a, SectionId>),
}

pub const SENTINEL_UNDEF: usize = usize::MAX;
pub trait RelocTarget {
    const SENTINEL_UNDEF: bool = false;
    /// Fixup a relocation entry with an invalid symbol. If this fixup fails,
    /// we warn and relocate to a tombstone address or error if tombstones
    /// are not enabled for this relocation context.
    fn fixup_reloc_entry(&self, entry: &RelocationEntry) -> Result<RelocationEntry> {
        Ok(entry.clone())
    }
    fn reloc_value(&self, reloc: RelocDetails<'_>) -> Result<Option<usize>>;
}

fn encode_leb128_u32_5byte(mut value: u32, buf: &mut [u8; 5]) {
    for b in &mut buf[0..5] {
        *b = (value as u8) & 0x7f;
        value >>= 7;
    }
    for b in &mut buf[0..4] {
        *b |= 0x80;
    }
}

fn encode_leb128_i32_5byte(mut value: i32, buf: &mut [u8; 5]) {
    for b in &mut buf[0..5] {
        *b = (value as u8) & 0x7f;
        value >>= 7;
    }
    for b in &mut buf[0..4] {
        *b |= 0x80;
    }
}

fn encode_leb128_u64_10byte(mut value: u64, buf: &mut [u8; 10]) {
    for b in &mut buf[0..10] {
        *b = (value as u8) & 0x7f;
        value >>= 7;
    }
    for b in &mut buf[0..9] {
        *b |= 0x80;
    }
}

fn encode_leb128_i64_10byte(mut value: i64, buf: &mut [u8; 10]) {
    for b in &mut buf[0..10] {
        *b = (value as u8) & 0x7f;
        value >>= 7;
    }
    for b in &mut buf[0..9] {
        *b |= 0x80;
    }
}

fn encode_u32(value: u32, buf: &mut [u8; 4]) {
    *buf = value.to_le_bytes();
}

fn encode_u64(value: u64, buf: &mut [u8; 8]) {
    *buf = value.to_le_bytes();
}

fn encode_for_ty(
    ty: RelocationType,
    value: usize,
    addend: isize,
    target: &mut [u8],
    allow_undef: bool,
) -> Result<()> {
    use RelocationType::*;
    let resolved = if allow_undef && value == SENTINEL_UNDEF {
        Some(SENTINEL_UNDEF)
    } else {
        value.checked_add_signed(addend)
    };
    let Some(resolved) = resolved else {
        bail!("reloc {ty:?} <{value:x}{addend:+}> overflows")
    };
    macro_rules! try_into_value {
        ($resolved:ident as $t:ty, $msg:literal) => {
            match $resolved {
                SENTINEL_UNDEF if allow_undef => -1isize as $t,
                resolved if let Ok(resolved) = resolved.try_into() => resolved,
                resolved => bail!("{}: {resolved:x}", $msg),
            }
        };
    }
    match ty {
        TableIndexI32 | MemoryAddrI32 | FunctionOffsetI32 | SectionOffsetI32 | GlobalIndexI32
        | FunctionIndexI32 | MemoryAddrLocrelI32 => {
            let resolved = try_into_value!(resolved as u32, "invalid value for I32 relocation");
            encode_u32(resolved, target.try_into().unwrap());
            Ok(())
        }
        FunctionIndexLeb | MemoryAddrLeb | TypeIndexLeb | GlobalIndexLeb | EventIndexLeb
        | TableNumberLeb => {
            let resolved = try_into_value!(resolved as u32, "invalid value for leb relocation");
            encode_leb128_u32_5byte(resolved, target.try_into().unwrap());
            Ok(())
        }
        TableIndexSleb | MemoryAddrSleb | MemoryAddrRelSleb | TableIndexRelSleb
        | MemoryAddrTlsSleb => {
            let resolved = try_into_value!(resolved as i32, "invalid value for sleb relocation");
            encode_leb128_i32_5byte(resolved, target.try_into().unwrap());
            Ok(())
        }
        FunctionOffsetI64 | MemoryAddrI64 | TableIndexI64 => {
            let resolved = try_into_value!(resolved as u64, "invalid value for I64 relocation");
            encode_u64(resolved, target.try_into().unwrap());
            Ok(())
        }
        MemoryAddrLeb64 => {
            let resolved = try_into_value!(resolved as u64, "invalid value for leb64 relocation");
            encode_leb128_u64_10byte(resolved, target.try_into().unwrap());
            Ok(())
        }
        MemoryAddrRelSleb64 | TableIndexSleb64 | TableIndexRelSleb64 | MemoryAddrTlsSleb64
        | MemoryAddrSleb64 => {
            let resolved = try_into_value!(resolved as i64, "invalid value for sleb64 relocation");
            encode_leb128_i64_10byte(resolved, target.try_into().unwrap());
            Ok(())
        }
    }
}
