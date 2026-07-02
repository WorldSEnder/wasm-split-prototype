use eyre::{bail, ensure, Result};
use gimli::DwarfSections;
use std::{collections::HashMap, fmt::Debug, ops::Range};
use wasmparser::{
    BinaryReader, CustomSectionReader, Imports, KnownCustom, NameSectionReader, Payload,
    ProducersSectionReader, Subsection, Subsections, TypeRef,
};
pub use wasmparser::{
    Data, Element, Export, FuncType, FunctionBody, Global, Import, MemoryType, Table, TagType,
};

use crate::{
    dep_graph::DepNode,
    magic_constants::{
        FeatureTag as WsFeature, SubsectionTag as WsSubsectionTag, Version as WsVersion,
    },
    reloc::{RelocInfo, RelocInfoParser},
};

pub type FuncTypeId = usize;
pub type InputFuncId = usize;
pub type TableId = usize;
pub type ImportId = usize;
pub type ExportId = usize;
pub type MemoryId = usize;
pub type GlobalId = usize;
pub type ElementId = usize;
pub type DataSegmentId = usize;
pub type TagId = usize;

#[derive(Debug)]
pub struct ImportedFunc {
    pub type_id: FuncTypeId,
    pub import_id: ImportId,
}

#[derive(Debug)]
pub struct DefinedFunc<'a> {
    pub type_id: FuncTypeId,
    pub body: FunctionBody<'a>,
}

#[derive(Default, Clone)]
pub struct Names<'a> {
    pub module: Option<&'a str>,
    pub functions: HashMap<InputFuncId, &'a str>,
    pub locals: HashMap<InputFuncId, wasmparser::NameMap<'a>>,
    pub labels: HashMap<InputFuncId, wasmparser::NameMap<'a>>,
    pub types: HashMap<FuncTypeId, &'a str>,
    pub tables: HashMap<TableId, &'a str>,
    pub memories: HashMap<MemoryId, &'a str>,
    pub globals: HashMap<GlobalId, &'a str>,
    pub elements: HashMap<ElementId, &'a str>,
    pub data_segments: HashMap<DataSegmentId, &'a str>,
    pub tags: HashMap<TagId, &'a str>,
}

fn convert_name_map<'a>(name_map: wasmparser::NameMap<'a>) -> Result<HashMap<usize, &'a str>> {
    name_map
        .into_iter()
        .map(|r| r.map(|naming| (naming.index as usize, naming.name)))
        .collect::<Result<HashMap<usize, &'a str>, _>>()
        .map_err(|e| e.into())
}

fn convert_indirect_name_map<'a>(
    indirect_name_map: wasmparser::IndirectNameMap<'a>,
) -> Result<HashMap<usize, wasmparser::NameMap<'a>>> {
    indirect_name_map
        .into_iter()
        .map(|r| -> Result<(usize, wasmparser::NameMap<'a>)> {
            let indirect_naming = r?;
            Ok((indirect_naming.index as usize, indirect_naming.names))
        })
        .collect::<Result<HashMap<_, _>, _>>()
}

impl<'a> Names<'a> {
    fn from_reader(rdr: NameSectionReader<'a>) -> Result<Self> {
        let mut names: Self = Default::default();
        for part in rdr {
            use wasmparser::Name;
            match part? {
                Name::Module { name, .. } => {
                    names.module = Some(name);
                }
                Name::Function(name_map) => {
                    names.functions = convert_name_map(name_map)?;
                }
                Name::Local(indirect_name_map) => {
                    names.locals = convert_indirect_name_map(indirect_name_map)?;
                }
                Name::Label(indirect_name_map) => {
                    names.labels = convert_indirect_name_map(indirect_name_map)?;
                }
                Name::Type(name_map) => {
                    names.types = convert_name_map(name_map)?;
                }
                Name::Table(name_map) => {
                    names.tables = convert_name_map(name_map)?;
                }
                Name::Memory(name_map) => {
                    names.memories = convert_name_map(name_map)?;
                }
                Name::Global(name_map) => {
                    names.globals = convert_name_map(name_map)?;
                }
                Name::Data(name_map) => {
                    names.data_segments = convert_name_map(name_map)?;
                }
                Name::Element(name_map) => {
                    names.elements = convert_name_map(name_map)?;
                }
                Name::Tag(name_map) => {
                    names.tags = convert_name_map(name_map)?;
                }
                Name::Field(_name_map) => {
                    bail!("Field names not supported");
                }
                Name::Unknown { ty, .. } => {
                    bail!("Unknown name subsection: {:?}", ty);
                }
            }
        }
        Ok(names)
    }
}

pub type InputOffset = usize;
pub use crate::reloc::SymbolIndex;

// We use our own struct here instead of a simple slice to track input positions and ranges
#[derive(Clone)]
pub struct DwarfReader<'a> {
    data: &'a [u8],
    input_position: usize,
}
impl Default for DwarfReader<'_> {
    fn default() -> Self {
        Self {
            data: &[],
            input_position: 0,
        }
    }
}
// TODO(MSRV): use std::fmt::from_fn
struct DebugByte(u8);
impl Debug for DebugByte {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:02x}", self.0)
    }
}
struct DebugLen(usize);
impl Debug for DebugLen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "...; {}", self.0)
    }
}
struct DebugBytes<'a>(&'a [u8]);
impl Debug for DebugBytes<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut list = f.debug_list();
        list.entries(self.0.iter().take(8).copied().map(DebugByte));
        if self.0.len() > 8 {
            list.entry(&DebugLen(self.0.len()));
        }
        list.finish()
    }
}
impl Debug for DwarfReader<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DwarfReader")
            .field("data", &DebugBytes(self.data))
            .field("pos", &self.input_position)
            .finish()
    }
}
impl DwarfReader<'_> {
    fn offset_id_enc(&self, offset: usize) -> u64 {
        self.data.as_ptr().wrapping_byte_add(offset) as u64
    }
    fn offset_id_dec(id: u64) -> *const u8 {
        std::ptr::null::<u8>().wrapping_byte_offset(id as isize)
    }
    fn offset_of_addr(&self, ptr: *const u8) -> Option<usize> {
        let offset = ptr as isize - self.data.as_ptr() as isize;
        // TODO(MSRV): diff.cast_unsigned/diff.strict_cast_unsigned
        if offset >= 0 && offset as usize <= gimli::Reader::len(self) {
            Some(offset as usize)
        } else {
            None
        }
    }
    /// range in the input of the byte range
    pub fn range(&self) -> Range<usize> {
        self.input_position..self.input_position + self.data.len()
    }
}
impl<'a> From<CustomSectionReader<'a>> for DwarfReader<'a> {
    fn from(custom: CustomSectionReader<'a>) -> Self {
        DwarfReader {
            data: custom.data(),
            input_position: custom.data_offset(),
        }
    }
}
impl<'a> gimli::Reader for DwarfReader<'a> {
    type Endian = gimli::LittleEndian;
    type Offset = usize;

    fn endian(&self) -> Self::Endian {
        gimli::LittleEndian
    }

    fn len(&self) -> Self::Offset {
        self.data.len()
    }

    fn empty(&mut self) {
        let _ = self.truncate(0);
    }

    fn truncate(&mut self, len: Self::Offset) -> gimli::Result<()> {
        let prefix = self.split(len)?;
        *self = prefix;
        Ok(())
    }

    fn offset_from(&self, base: &Self) -> Self::Offset {
        let offset = base.offset_of_addr(self.data.as_ptr()).unwrap();
        assert!(offset + self.len() <= base.len());
        offset
    }

    fn offset_id(&self) -> gimli::ReaderOffsetId {
        gimli::ReaderOffsetId(self.offset_id_enc(0))
    }

    fn lookup_offset_id(&self, id: gimli::ReaderOffsetId) -> Option<Self::Offset> {
        let ptr = Self::offset_id_dec(id.0);
        let offset = ptr as isize - self.data.as_ptr() as isize;
        if offset >= 0 && offset as usize <= self.len() {
            Some(offset as usize)
        } else {
            None
        }
    }

    fn find(&self, byte: u8) -> gimli::Result<Self::Offset> {
        self.data
            .iter()
            .position(|&c| c == byte)
            .ok_or_else(|| gimli::Error::UnexpectedEof(self.offset_id()))
    }

    fn skip(&mut self, len: Self::Offset) -> gimli::Result<()> {
        let _ = self.split(len)?;
        Ok(())
    }

    fn split(&mut self, len: Self::Offset) -> gimli::Result<Self> {
        if !(len <= self.data.len()) {
            return Err(gimli::Error::UnexpectedEof(self.offset_id()));
        }
        let (prefix, more) = self.data.split_at(len);
        *self = Self {
            data: more,
            input_position: self.input_position + len,
        };
        Ok(Self {
            data: prefix,
            input_position: self.input_position,
        })
    }

    fn to_slice(&self) -> gimli::Result<std::borrow::Cow<'_, [u8]>> {
        Ok(self.data.into())
    }

    fn to_string(&self) -> gimli::Result<std::borrow::Cow<'_, str>> {
        match str::from_utf8(self.data) {
            Ok(s) => Ok(s.into()),
            _ => Err(gimli::Error::BadUtf8),
        }
    }

    fn to_string_lossy(&self) -> gimli::Result<std::borrow::Cow<'_, str>> {
        Ok(String::from_utf8_lossy(self.data))
    }

    fn read_slice(&mut self, buf: &mut [u8]) -> gimli::Result<()> {
        let prefix = self.split(buf.len())?;
        buf.copy_from_slice(prefix.data);
        Ok(())
    }
}

pub enum DwarfState<'a> {
    None,
    Inline(DwarfSections<DwarfReader<'a>>),
    External,
}

impl Default for DwarfState<'_> {
    fn default() -> Self {
        Self::None
    }
}

impl<'a> From<DwarfParseState<'a>> for DwarfState<'a> {
    fn from(value: DwarfParseState<'a>) -> Self {
        match value {
            DwarfParseState::Undecided => Self::None,
            DwarfParseState::Inline(sections) => Self::Inline(sections),
            DwarfParseState::External => Self::External,
        }
    }
}

impl DwarfState<'_> {
    pub fn print_fully(&self) {
        let Self::Inline(dwarf) = &self else {
            return;
        };
        if !tracing::event_enabled!(tracing::Level::WARN) {
            return;
        }
        tracing::warn!("{dwarf:?}");
    }
}

enum DwarfParseState<'a> {
    Undecided,
    Inline(DwarfSections<DwarfReader<'a>>),
    External,
}

impl Default for DwarfParseState<'_> {
    fn default() -> Self {
        Self::Undecided
    }
}

impl<'a> DwarfParseState<'a> {
    fn as_internal(&mut self) -> Option<&mut DwarfSections<DwarfReader<'a>>> {
        if let Self::Undecided = self {
            *self = Self::Inline(DwarfSections::default());
        }
        match self {
            Self::Inline(sections) => Some(sections),
            _ => None,
        }
    }
    fn as_external(&mut self) -> Option<()> {
        if let Self::Undecided = self {
            *self = Self::External;
        }
        match self {
            Self::External => Some(()),
            _ => None,
        }
    }
}

#[derive(Default)]
pub struct InputModule<'a> {
    pub raw: &'a [u8],
    pub types: Vec<FuncType>,
    pub imports: Vec<Import<'a>>,
    // imports take up an index, hence we keep track of imports vs defined for each
    pub imported_tags_num: usize,
    pub tags: Vec<TagType>,

    pub imported_globals_num: usize,
    pub globals: Vec<Global<'a>>,

    pub imported_memories_num: usize,
    pub memories: Vec<MemoryType>,

    pub imported_tables_num: usize,
    pub tables: Vec<Table<'a>>,

    // functions get their own code section, the function section only has type definitions
    pub start: Option<InputFuncId>,

    pub elements: Vec<Element<'a>>,

    pub data_segments: Vec<Data<'a>>,

    pub imported_funcs: Vec<ImportedFunc>,
    pub defined_funcs: Vec<DefinedFunc<'a>>,

    pub exports: Vec<Export<'a>>,

    // interesting custom sections
    pub producers: Option<ProducersSectionReader<'a>>,
    pub target_features: Option<CustomSectionReader<'a>>,
    pub wasm_bindgen_unstable: Vec<CustomSectionReader<'a>>,
    pub dwarf: DwarfState<'a>,

    pub names: Names<'a>,
    pub imported_func_map: HashMap<ImportId, InputFuncId>,

    pub reloc_info: RelocInfo<'a>,
    pub options: Options,
}

pub enum Strictness {
    IntegrationTesting,
    Lenient,
}

impl<'a> InputModule<'a> {
    pub fn parse(wasm: &'a [u8], strict: Strictness) -> Result<Self> {
        let mut module = Self {
            raw: wasm,
            ..Default::default()
        };
        let mut reloc_info = RelocInfoParser::default();
        let mut function_types: Vec<FuncTypeId> = Vec::new();
        let parser = wasmparser::Parser::new(0);
        let mut dwarf_state = DwarfParseState::default();
        let mut num_split_sections_found = 0;
        for payload in parser.parse_all(wasm) {
            let payload = payload?;
            let was_reloc_section = reloc_info.visit_payload(&payload)?;
            match payload {
                Payload::Version { .. } => {}
                Payload::TypeSection(reader) => {
                    module.types = reader
                        .into_iter_err_on_gc_types()
                        .collect::<Result<Vec<_>, _>>()?;
                }
                Payload::ImportSection(reader) => {
                    let gathered = &mut module.imports;
                    for imports in reader.into_iter() {
                        match imports? {
                            Imports::Single(_, import) => gathered.push(import),
                            Imports::Compact1 { module, items } => {
                                for item in items.into_iter() {
                                    let wasmparser::ImportItemCompact { name, ty } = item?;
                                    gathered.push(Import { module, name, ty })
                                }
                            }
                            Imports::Compact2 { module, ty, names } => {
                                for name in names.into_iter() {
                                    let name = name?;
                                    gathered.push(Import { module, name, ty })
                                }
                            }
                        }
                    }
                }
                Payload::FunctionSection(reader) => {
                    function_types = reader
                        .into_iter()
                        .map(|t| t.map(|id| id as FuncTypeId))
                        .collect::<Result<Vec<_>, _>>()?;
                }
                Payload::TableSection(reader) => {
                    module.tables = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::MemorySection(reader) => {
                    module.memories = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::TagSection(reader) => {
                    module.tags = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::GlobalSection(reader) => {
                    module.globals = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::ExportSection(reader) => {
                    module.exports = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::StartSection { func, .. } => {
                    module.start = Some(func as usize);
                }
                Payload::ElementSection(reader) => {
                    module.elements = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::DataCountSection { .. } => {}
                Payload::DataSection(reader) => {
                    module.data_segments = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
                }
                Payload::CodeSectionStart { .. } => {}
                Payload::CodeSectionEntry(body) => {
                    let index = module.defined_funcs.len();
                    module.defined_funcs.push(DefinedFunc {
                        type_id: function_types[index],
                        body,
                    });
                }
                Payload::CustomSection(reader) => {
                    macro_rules! dwarf_match {
                        ($name:ident) => {{
                            let Some(dwarf) = dwarf_state.as_internal() else {
                                bail!("can't have dwarf sections with external dwarf data");
                            };
                            dwarf.$name = DwarfReader::from(reader).into();
                        }};
                    }
                    match (reader.as_known(), reader.name()) {
                        (KnownCustom::Name(names), _) => module.names = Names::from_reader(names)?,
                        (KnownCustom::Producers(producers), _) => {
                            module.producers = Some(producers);
                        }
                        (_, "target_features") => {
                            module.target_features = Some(reader);
                        }
                        (_, "__wasm_bindgen_unstable") => {
                            module.wasm_bindgen_unstable.push(reader);
                        }
                        (_, crate::magic_constants::LINK_SECTION) => {
                            let rdr = BinaryReader::new(reader.data(), reader.data_offset());
                            let () = read_wasm_split_section(rdr, &mut module.options)?;
                            num_split_sections_found += 1;
                        }
                        (_, ".debug_abbrev") => dwarf_match!(debug_abbrev),
                        (_, ".debug_addr") => dwarf_match!(debug_addr),
                        (_, ".debug_aranges") => dwarf_match!(debug_aranges),
                        (_, ".debug_info") => dwarf_match!(debug_info),
                        (_, ".debug_line") => dwarf_match!(debug_line),
                        (_, ".debug_line_str") => dwarf_match!(debug_line_str),
                        (_, ".debug_macinfo") => dwarf_match!(debug_macinfo),
                        (_, ".debug_macro") => dwarf_match!(debug_macro),
                        (_, ".debug_names") => dwarf_match!(debug_names),
                        (_, ".debug_str") => dwarf_match!(debug_str),
                        (_, ".debug_str_offsets") => dwarf_match!(debug_str_offsets),
                        (_, ".debug_types") => dwarf_match!(debug_types),
                        (_, ".debug_loc") => dwarf_match!(debug_loc),
                        (_, ".debug_loclists") => dwarf_match!(debug_loclists),
                        (_, ".debug_ranges") => dwarf_match!(debug_ranges),
                        (_, ".debug_rnglists") => dwarf_match!(debug_rnglists),
                        // https://github.com/WebAssembly/tool-conventions/blob/main/Dwarf.md
                        (_, "external_debug_info") => {
                            ensure!(
                                dwarf_state.as_external().is_some(),
                                "can't have both external and internal dwarf data"
                            );
                            // TODO: the file location "should" be relative to the input file
                            // currently, we don't do any file loading in this tool though,
                            // there is no protocol either to ask the caller about this either.
                            tracing::warn!(
                                "external DWARF data detected that will not be relocated"
                            );
                        }
                        _ => {
                            if !was_reloc_section {
                                tracing::warn!(
                                    "Ignoring custom section {} at [{:x}]",
                                    reader.name(),
                                    reader.data_offset()
                                );
                            }
                        }
                    };
                }
                Payload::End(_) => {}
                section => {
                    tracing::warn!("Ignoring unknown section {section:?}");
                }
            }
        }

        if matches!(strict, Strictness::IntegrationTesting) && num_split_sections_found != 1 {
            bail!(
                "Wrong number of custom sections for wasm-split found, expected 1 got {}",
                num_split_sections_found
            );
        }
        module.reloc_info = reloc_info.finish(&module)?;
        module.dwarf = dwarf_state.into();

        for (import_id, import) in module.imports.iter().enumerate() {
            let import_id = import_id as ImportId;
            match import.ty {
                TypeRef::Func(func_type_id) | TypeRef::FuncExact(func_type_id) => {
                    let func_id = module.imported_funcs.len();
                    module.imported_funcs.push(ImportedFunc {
                        import_id,
                        type_id: func_type_id as usize,
                    });
                    module.imported_func_map.insert(import_id, func_id);
                }
                TypeRef::Table(_table_type) => module.imported_tables_num += 1,
                TypeRef::Memory(_memory_type) => module.imported_memories_num += 1,
                TypeRef::Global(_global_type) => module.imported_globals_num += 1,
                TypeRef::Tag(_tag_type) => module.imported_tags_num += 1,
            }
        }
        Ok(module)
    }

    pub fn func_type_id(&self, func_id: InputFuncId) -> FuncTypeId {
        if func_id < self.imported_funcs.len() {
            self.imported_funcs[func_id].type_id
        } else {
            self.defined_funcs[func_id - self.imported_funcs.len()].type_id
        }
    }

    pub fn main_memory(&self) -> DepNode {
        DepNode::Memory(0)
    }

    pub fn indirect_function_table(&self) -> DepNode {
        DepNode::Table(self.reloc_info.indirect_table)
    }
}

enum WsReadVersion {
    Known(WsVersion),
    Unknown,
}
enum WsReadFeature {
    Known(WsFeature),
    Unknown,
}
enum WasmSplitSubsection<'a> {
    Version(WsReadVersion),
    Feature(WsReadFeature),
    Unknown(BinaryReader<'a>),
}
fn read_version(reader: &mut BinaryReader<'_>) -> wasmparser::Result<WsReadVersion> {
    let version = reader.read_u8()?;
    // TODO: for future version, read an uleb
    Ok(match version {
        vers if vers == (WsVersion::Version1 as u8) => WsReadVersion::Known(WsVersion::Version1),
        _ => WsReadVersion::Unknown,
    })
}
fn read_feature(reader: &mut BinaryReader<'_>) -> wasmparser::Result<WsReadFeature> {
    let feature = reader.read_u8()?;
    // TODO: for future version, read an uleb
    Ok(match feature {
        feat if feat == WsFeature::CfgDebugAssertions as u8 => {
            WsReadFeature::Known(WsFeature::CfgDebugAssertions)
        }
        _ => WsReadFeature::Unknown,
    })
}

impl<'a> Subsection<'a> for WasmSplitSubsection<'a> {
    fn from_reader(id: u8, mut reader: BinaryReader<'a>) -> wasmparser::Result<Self> {
        match id {
            id if id == WsSubsectionTag::Version as u8 => {
                let version = read_version(&mut reader)?;
                Ok(Self::Version(version))
            }
            id if id == WsSubsectionTag::Feature as u8 => {
                let feature = read_feature(&mut reader)?;
                Ok(Self::Feature(feature))
            }
            _ => Ok(Self::Unknown(reader)),
        }
    }
}

/// Options found in the web assembly.
// Make sure these do not depend on the order they are found in.
#[derive(Default)]
pub struct Options {
    pub debug_assertions: bool,
}

fn read_wasm_split_section(rdr: BinaryReader<'_>, options: &mut Options) -> Result<()> {
    let subsections = Subsections::<WasmSplitSubsection>::new(rdr);
    for subsection in subsections {
        match subsection? {
            WasmSplitSubsection::Version(WsReadVersion::Unknown) => bail!(
                r#"Input file was linked with a `wasm_split_helpers` version that is not recognized by this tool. \
                Please upgrade your `wasm_split_cli` version and ensure you have the latest version of the relevant \
                build tools installed."#
            ),
            WasmSplitSubsection::Version(WsReadVersion::Known(WsVersion::Version1)) => {
                // No additional information at the moment
            }
            // features are by design optional to detect!
            WasmSplitSubsection::Feature(WsReadFeature::Unknown) => {}
            WasmSplitSubsection::Feature(WsReadFeature::Known(WsFeature::CfgDebugAssertions)) => {
                options.debug_assertions = true;
            }
            WasmSplitSubsection::Unknown(reader) => {
                let _ = reader;
                // Ignore all subsections that are unrecognized for upwards compatibility
            }
        }
    }
    Ok(())
}
