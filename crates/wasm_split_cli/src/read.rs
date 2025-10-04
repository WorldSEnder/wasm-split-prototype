use eyre::{bail, Result};
use std::collections::HashMap;
use wasmparser::{BinaryReader, NameSectionReader, Payload, TypeRef};
pub use wasmparser::{
    Data, Element, Export, FuncType, FunctionBody, Global, Import, MemoryType, Table, TagType,
};

use crate::reloc::{RelocInfo, RelocInfoParser};

pub struct CustomSection<'a> {
    pub name: &'a str,
    pub data_offset: usize,
    pub data: &'a [u8],
}

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
    fn new(data: &'a [u8], original_offset: usize) -> Result<Self> {
        let mut names: Self = Default::default();
        for part in NameSectionReader::new(BinaryReader::new(data, original_offset)) {
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

    pub custom_sections: Vec<CustomSection<'a>>,

    pub names: Names<'a>,
    pub imported_func_map: HashMap<ImportId, InputFuncId>,

    pub reloc_info: RelocInfo<'a>,
}

impl<'a> InputModule<'a> {
    pub fn parse(wasm: &'a [u8]) -> Result<Self> {
        let mut module = Self {
            raw: wasm,
            ..Default::default()
        };
        let mut reloc_info = RelocInfoParser::default();
        let mut function_types: Vec<FuncTypeId> = Vec::new();
        let parser = wasmparser::Parser::new(0);
        for payload in parser.parse_all(wasm) {
            let payload = payload?;
            reloc_info.visit_payload(&payload)?;
            match payload {
                Payload::Version { .. } => {}
                Payload::TypeSection(reader) => {
                    module.types = reader
                        .into_iter_err_on_gc_types()
                        .collect::<Result<Vec<_>, _>>()?;
                }
                Payload::ImportSection(reader) => {
                    module.imports = reader.into_iter().collect::<Result<Vec<_>, _>>()?;
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
                    module.custom_sections.push(CustomSection {
                        name: reader.name(),
                        data: reader.data(),
                        data_offset: reader.data_offset(),
                    });
                }
                Payload::End(_) => {}
                section => {
                    bail!("Unknown section: {:?}", section);
                }
            }
        }

        for section in module.custom_sections.iter() {
            if section.name == "name" {
                module.names = Names::new(section.data, section.data_offset)?;
            }
        }
        module.reloc_info = reloc_info.finish(&module)?;

        for (import_id, import) in module.imports.iter().enumerate() {
            let import_id = import_id as ImportId;
            match import.ty {
                TypeRef::Func(func_type_id) => {
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
}
