use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

use eyre::{anyhow, bail, Result};
use tracing::trace;
use wasmparser::{
    BinaryReader, CustomSectionReader, Data, DefinedDataSymbol, ElementItems, ElementKind, Linking,
    LinkingSectionReader, Payload, RelocAddendKind, RelocSectionReader, RelocationEntry,
    RelocationType, Segment, SymbolFlags, SymbolInfo,
};

use crate::{
    read::{GlobalId, InputFuncId, InputModule, InputOffset, TableId, TagId},
    util::find_subrange,
};

// An offset (index) into the bytes of the input module
pub type SectionIndex = usize;
pub type SymbolIndex = usize;

#[derive(Default)]
pub struct RelocInfoParser<'a> {
    info: RelocInfo<'a>,
    // We NEED this to be present to identify the table to fix-up
    indirect_function_table: Option<TableId>,
    stack_pointer: Option<GlobalId>,
    tls_base: Option<GlobalId>,
}

impl<'a> RelocInfoParser<'a> {
    fn visit_custom(&mut self, custom: &CustomSectionReader<'a>) -> Result<()> {
        if custom.name() == "linking" {
            let reader =
                LinkingSectionReader::new(BinaryReader::new(custom.data(), custom.data_offset()))?;
            let reader = reader.subsections();
            for subsection in reader {
                let subsection = subsection?;
                if let Linking::SegmentInfo(segments) = subsection {
                    assert!(self.info.segments.is_empty(), "duplicate segments info");
                    self.info.segments = segments.into_iter().collect::<Result<_, _>>()?;
                    continue;
                }
                if let Linking::SymbolTable(map) = subsection {
                    assert!(self.info.symbols.is_empty(), "duplicate symbol table");
                    self.info.symbols = map.into_iter().collect::<Result<Vec<_>, _>>()?;
                    for sym in &self.info.symbols {
                        match *sym {
                            SymbolInfo::Table {
                                name: Some("__indirect_function_table"),
                                index,
                                ..
                            } => {
                                self.indirect_function_table = Some(index as TableId);
                            }
                            SymbolInfo::Global {
                                name: Some("__stack_pointer"),
                                index,
                                ..
                            } => {
                                self.stack_pointer = Some(index as GlobalId);
                            }
                            SymbolInfo::Global {
                                name: Some("__tls_base"),
                                index,
                                ..
                            } => {
                                self.tls_base = Some(index as GlobalId);
                            }

                            _ => {}
                        }
                    }
                }
            }
        } else if custom.name().starts_with("reloc.") {
            let reader =
                RelocSectionReader::new(BinaryReader::new(custom.data(), custom.data_offset()))?;
            let mut reloc_entries = reader
                .entries()
                .into_iter()
                .collect::<Result<Vec<_>, _>>()?;
            reloc_entries.sort_by_key(|entry| entry.offset);
            self.info
                .relocs
                .insert(reader.section_index() as SectionIndex, reloc_entries);
        }
        Ok(())
    }
    pub fn visit_payload(&mut self, payload: &Payload<'a>) -> Result<()> {
        let section_index = self.info.section_ranges.len();
        if let Some((_, section_range)) = payload.as_section() {
            self.info.section_ranges.push(section_range);
        }
        match payload {
            Payload::DataSection(_) => {
                self.info.data_section_index = section_index;
            }
            Payload::CodeSectionStart { .. } => {
                self.info.code_section_index = section_index;
            }
            Payload::CustomSection(reader) => self.visit_custom(reader)?,
            _ => {}
        };
        Ok(())
    }
    pub fn finish(self, module: &InputModule<'a>) -> Result<RelocInfo<'a>> {
        let mut info = self.info;
        info.data_symbols = get_data_symbols(&module.data_segments, &info.symbols)?;
        // We may be able to get away with zero here, but don't!
        let Some(indirect_function_table) = self.indirect_function_table else {
            bail!("No indirect function table found in the reloc data");
        };
        let Some(stack_pointer) = self.stack_pointer else {
            bail!("No stack pointer found in the reloc data");
        };
        info.indirect_table = indirect_function_table;
        info.stack_pointer = stack_pointer;
        get_indirect_functions(&mut info, indirect_function_table, module)?;
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
    for relocation in this.relocs.iter().flat_map(|(_, relocs)| relocs.iter()) {
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
        if symbol
            .offset
            .checked_add(symbol.size)
            .ok_or_else(|| anyhow!("Invalid symbol: {symbol:?}"))? as usize
            > data_segment.data.len()
        {
            bail!(
                "Invalid symbol {symbol:?} for data segment of size {:?}",
                data_segment.data.len()
            );
        }
        let offset = data_segment.range.end - data_segment.data.len() + (symbol.offset as usize);
        let range = offset..(offset + symbol.size as usize);
        data_symbols.push(DataSymbol {
            symbol_index,
            range,
        });
    }
    data_symbols.sort_by_key(|symbol| symbol.range.start);
    Ok(data_symbols)
}

#[derive(Default)]
pub struct RelocInfo<'a> {
    pub section_ranges: Vec<Range<InputOffset>>,
    pub segments: Vec<Segment<'a>>,

    pub code_section_index: SectionIndex,
    pub data_section_index: SectionIndex,
    pub data_symbols: Vec<DataSymbol>,
    pub symbols: Vec<SymbolInfo<'a>>,
    pub relocs: HashMap<usize, Vec<RelocationEntry>>,

    pub indirect_table: TableId,
    pub stack_pointer: GlobalId,
    pub visible_indirects: HashSet<InputFuncId>,
    pub referenced_indirects: HashSet<InputFuncId>,
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
            trace!("Relocs in {section} >>>>>>>>>>>>>>>>>>>>>>>>");
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
                trace!(symbol_info);
            }
            trace!("Relocs in {section} <<<<<<<<<<<<<<<<<<<<<<<<");
        }
    }
    pub fn section_offset(&self, section: SectionIndex) -> InputOffset {
        self.section_ranges[section].start
    }
    pub fn data_section_offset(&self) -> InputOffset {
        self.section_offset(self.data_section_index)
    }
    pub fn code_section_offset(&self) -> InputOffset {
        self.section_offset(self.code_section_index)
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
            &self.section_ranges,
            |section_range| section_range.end >= range.end,
            |section_range| section_range.start < range.end,
        );
        let section = target_sections.start;
        let section_range = &self.section_ranges[section];
        assert!(
            section_range.start <= range.start && range.end <= section_range.end,
            "range to rellocate should be fully contained in one section"
        );
        let section_subrange =
            (range.start - section_range.start)..(range.end - section_range.start);

        let section_relocs = self.iter_section_relocs(section);
        let reloc_range = find_subrange(
            section_relocs,
            |reloc| (reloc.offset as usize) >= section_subrange.start,
            |reloc| (reloc.offset as usize) < section_subrange.end,
        );

        (section_range.start, section_relocs[reloc_range].iter())
    }

    pub fn get_relocated_data(
        module: &InputModule,
        range: Range<InputOffset>,
        target: &impl RelocTarget,
    ) -> Result<Vec<u8>> {
        let this = &module.reloc_info;
        let mut data = Vec::from(&module.raw[range.clone()]);
        let (reloc_base, relocs) = this.get_relocations_for_range(&range);
        for relocation in relocs {
            this.apply_relocation(target, &mut data, range.start, reloc_base, relocation)?;
        }
        Ok(data)
    }

    pub fn expand_relocation(&self, relocation: &RelocationEntry) -> Result<RelocDetails<'_>> {
        use wasmparser::RelocationType::*;
        let symbol_index = relocation.index as usize;
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
            wasmparser::RelocationType::SectionOffsetI32
            | wasmparser::RelocationType::FunctionOffsetI32
            | wasmparser::RelocationType::FunctionOffsetI64 => {
                bail!(
                    "unhandled relocation ty {:?} module relocation",
                    relocation.ty
                );
            } // [relocate data segments]
              // TODO: there is no relocation for data segments. As such, we'd have to parse the opcodes to find
              // references to passive and declarative data segments. The solution: only handling active segments
              // and don't change the index of passive ones.
        }
    }

    fn apply_relocation(
        &self,
        reloc_target: &impl RelocTarget,
        data: &mut [u8],
        data_offset: InputOffset,
        reloc_base: InputOffset,
        relocation: &RelocationEntry,
    ) -> Result<()> {
        let relocation_range = relocation.relocation_range();
        let target = &mut data[(reloc_base + relocation_range.start - data_offset)
            ..(reloc_base + relocation_range.end - data_offset)];
        let ty = relocation.ty;
        let relocated = reloc_target.reloc_value(self.expand_relocation(relocation)?)?;
        let Some(value) = relocated else {
            return Ok(());
        };
        // handle overflow maybe? Not sure if wrapping would be correct
        let value: i64 = value.try_into().expect("relocated value too big");
        debug_assert!(
            relocation.addend == 0 || ty.addend_kind() != RelocAddendKind::None,
            "relocation {relocation:?} without addend should have addend == 0, not {}",
            relocation.addend,
        );
        let value = value + relocation.addend;
        encode_for_ty(ty)(value, target);
        Ok(())
    }
}

pub struct SymbolDetails<'a, Idx> {
    pub _symbol_index: usize,
    pub index: Idx,
    pub _flags: SymbolFlags,
    pub _name: Option<&'a str>,
}

pub struct DataDetails<'a> {
    pub symbol_index: usize,
    pub _flags: SymbolFlags,
    pub _name: &'a str,
    pub definition: Option<&'a DefinedDataSymbol>,
}

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
}

pub trait RelocTarget {
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

fn encode_for_ty(ty: RelocationType) -> fn(i64, &mut [u8]) {
    use RelocationType::*;
    match ty {
        TableIndexI32 | MemoryAddrI32 | FunctionOffsetI32 | SectionOffsetI32 | GlobalIndexI32
        | FunctionIndexI32 | MemoryAddrLocrelI32 => |value, target| {
            encode_u32(
                value.try_into().expect("invalid value for I32 relocation"),
                target.try_into().unwrap(),
            )
        },
        FunctionIndexLeb | MemoryAddrLeb | TypeIndexLeb | GlobalIndexLeb | EventIndexLeb
        | TableNumberLeb => |value, target| {
            encode_leb128_u32_5byte(
                value.try_into().expect("invalid value for leb relocation"),
                target.try_into().unwrap(),
            );
        },
        TableIndexSleb | MemoryAddrSleb | MemoryAddrRelSleb | TableIndexRelSleb
        | MemoryAddrTlsSleb => |value, target| {
            encode_leb128_i32_5byte(
                value.try_into().expect("invalid value for sleb relocation"),
                target.try_into().unwrap(),
            );
        },
        FunctionOffsetI64 | MemoryAddrI64 | TableIndexI64 => |value, target| {
            encode_u64(
                value.try_into().expect("invalid value for I64 relocation"),
                target.try_into().unwrap(),
            );
        },
        MemoryAddrLeb64 => |value, target| {
            encode_leb128_u64_10byte(
                value
                    .try_into()
                    .expect("invalid value for leb64 relocation"),
                target.try_into().unwrap(),
            );
        },
        MemoryAddrRelSleb64 | TableIndexSleb64 | TableIndexRelSleb64 | MemoryAddrTlsSleb64
        | MemoryAddrSleb64 => |value, target| {
            encode_leb128_i64_10byte(value, target.try_into().unwrap());
        },
    }
}
