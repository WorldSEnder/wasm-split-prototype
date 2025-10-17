use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    ops::Range,
};

use crate::{
    dep_graph::DepNode,
    magic_constants,
    read::{InputFuncId, InputModule, InputOffset},
    reloc::{RelocDetails, RelocInfo, RelocTarget},
    split_point::SplitProgramInfo,
};
use eyre::{anyhow, bail, Context, Result};
use tracing::{trace, warn};
use wasm_encoder::{EntityType, ProducersField, ProducersSection};
use wasmparser::{
    BinaryReader, Data, DataKind, DefinedDataSymbol, ExternalKind, ProducersSectionReader,
    SegmentFlags, SymbolInfo, TypeRef,
};

struct EmitState<'a> {
    input_module: &'a InputModule<'a>,
    link_module: &'a str,
    // info about shared usage
    indirect_functions: IndirectFunctionEmitInfo,
    data_relocations: DataEmitInfo,
    shared_names: HashMap<DepNode, Cow<'a, str>>,
}

impl<'a> EmitState<'a> {
    fn new(
        module: &'a InputModule<'a>,
        program_info: &SplitProgramInfo,
        link_module: &'a str,
    ) -> Result<Self> {
        let indirect_functions = IndirectFunctionEmitInfo::new(module, program_info)?;
        let data_relocations = DataEmitInfo::new(module, program_info)?;

        let mut shared_names = HashMap::new();
        // we overwrite the mapping potentially by the order of the below, but that's okay.
        let mut unique_id = 0;
        for dep in &program_info.shared_deps {
            if let DepNode::Function(_) | DepNode::DataSymbol(_) = dep {
                continue;
            }
            let chosen_name = Cow::Owned(format!("__wasm_split_shared{unique_id}"));
            unique_id += 1;
            shared_names.insert(*dep, chosen_name);
        }

        shared_names.insert(
            DepNode::Global(module.reloc_info.stack_pointer),
            Cow::Borrowed("__stack_pointer"),
        );
        shared_names.insert(
            DepNode::Table(module.reloc_info.indirect_table),
            Cow::Borrowed("__indirect_function_table"),
        );
        shared_names.insert(DepNode::Memory(0), Cow::Borrowed("memory"));

        for export in &module.exports {
            let asdep = match export.kind {
                ExternalKind::Global => DepNode::Global(export.index as usize),
                ExternalKind::Table => DepNode::Table(export.index as usize),
                ExternalKind::Tag => DepNode::Tag(export.index as usize),
                ExternalKind::Memory => DepNode::Memory(export.index as usize),
                _ => continue,
            };
            shared_names.insert(asdep, Cow::Borrowed(export.name));
        }

        Ok(EmitState {
            input_module: module,
            link_module,
            indirect_functions,
            data_relocations,
            shared_names,
        })
    }
    fn get_indirect_function_table_type(&self) -> wasmparser::TableType {
        // + 1 due to empty entry at index 0
        let indirect_table_size = self.indirect_functions.table_entries.len() + 1;
        wasmparser::TableType {
            element_type: wasmparser::RefType::FUNCREF,
            initial: (indirect_table_size as u32).into(),
            maximum: None,
            table64: false,
            shared: false,
        }
    }
    fn name_for(&self, dep: &DepNode) -> Cow<'a, str> {
        match dep {
            dep @ DepNode::Global(_)
            | dep @ DepNode::Table(_)
            | dep @ DepNode::Tag(_)
            | dep @ DepNode::Memory(_) => self
                .shared_names
                .get(dep)
                .unwrap_or_else(|| panic!("a name for the dependency {dep:?} to be shared")),
            DepNode::DataSymbol(_) => {
                unreachable!("data symbols are shared via their memory address")
            }
            DepNode::Function(_) => {
                unreachable!("functions are shared via the indirect function table")
            }
        }
        .clone()
    }
    fn shared_import_for(&self, dep: &DepNode) -> OutputImport<'a> {
        let name = self.name_for(dep);
        let ty = match dep {
            DepNode::Global(global_id) => {
                // We dont remember the type of imported globals at the moment :/
                let global_index = global_id
                    .checked_sub(self.input_module.imported_globals_num)
                    .unwrap();
                TypeRef::Global(self.input_module.globals[global_index].ty)
            }
            DepNode::Table(table_id)
                if *table_id == self.input_module.reloc_info.indirect_table =>
            {
                TypeRef::Table(self.get_indirect_function_table_type())
            }
            DepNode::Table(table_id) => {
                // We dont remember the type of import tables at the moment :/
                let table_index = table_id
                    .checked_sub(self.input_module.imported_tables_num)
                    .unwrap();
                TypeRef::Table(self.input_module.tables[table_index].ty)
            }
            DepNode::Memory(memory_id) => {
                // We dont remember the type of import memories at the moment :/
                let memory_index = memory_id
                    .checked_sub(self.input_module.imported_memories_num)
                    .unwrap();
                let mem = &self.input_module.memories[memory_index];
                TypeRef::Memory(*mem)
            }
            _ => unreachable!("generating shared import for {dep:?} not supported."),
        };
        OutputImport {
            module: Cow::Borrowed("__wasm_split"),
            name,
            ty,
        }
    }
    fn shared_export_for(&self, dep: &DepNode) -> OutputExport<'a> {
        let name = self.name_for(dep);
        let (index, kind) = match dep {
            DepNode::Global(global_id) => (*global_id as u32, ExternalKind::Global),
            DepNode::Table(table_id) => (*table_id as u32, ExternalKind::Table),
            DepNode::Tag(tag_id) => (*tag_id as u32, ExternalKind::Tag),
            DepNode::Memory(memory_id) => (*memory_id as u32, ExternalKind::Memory),
            DepNode::DataSymbol(_) | DepNode::Function(_) => {
                unreachable!("generating shared export {dep:?} not supported.")
            }
        };
        OutputExport { name, kind, index }
    }
}

#[derive(Debug, Default)]
struct IndirectFunctionEmitInfo {
    table_entries: Vec<InputFuncId>,
    function_table_index: HashMap<InputFuncId, usize>,
    table_range_for_output_module: Vec<Range<usize>>,
}

impl IndirectFunctionEmitInfo {
    fn new(module: &InputModule, program_info: &SplitProgramInfo) -> Result<Self> {
        let mut indirect_functions = module
            .reloc_info
            .referenced_indirects
            .iter()
            .filter(|&&func| {
                program_info
                    .symbol_output_module
                    .contains_key(&DepNode::Function(func))
            })
            .cloned()
            .collect::<HashSet<_>>();
        indirect_functions.extend(program_info.shared_deps.iter().filter_map(|dep| match dep {
            DepNode::Function(f) => Some(f),
            _ => None,
        }));

        // Remove all split point imports. These are placeholders. Any
        // references to these functions will be replaced by a reference to the
        // corresponding `SplitPoint::export_func`.
        for (_, output_module) in program_info.output_modules.iter() {
            for split_point in output_module.split_points.iter() {
                indirect_functions.remove(&split_point.import_func);
            }
        }

        let module_for_func = |func_id| {
            *program_info
                .symbol_output_module
                .get(&DepNode::Function(func_id))
                .unwrap_or_else(|| panic!("No module for indirect function {func_id}"))
        };

        let mut table_entries: Vec<_> = indirect_functions.into_iter().collect();
        table_entries.sort_unstable_by_key(|&func_id| module_for_func(func_id));
        let function_table_index: HashMap<_, _> = table_entries
            .iter()
            .enumerate()
            .map(|(i, func_id)| (*func_id, i + 1))
            .collect();
        let end_table_index = table_entries.len() + 1;

        let mut func_it = table_entries.iter().enumerate().peekable();
        let table_range_for_output_module = program_info
            .output_modules
            .iter()
            .enumerate()
            .map(|(output_module_index, _)| {
                let mod_start = func_it
                    .peek()
                    .map(|(i, _)| i + 1)
                    .unwrap_or(end_table_index);
                while func_it
                    .next_if(|&(_, &f)| output_module_index == module_for_func(f))
                    .is_some()
                {}
                let mod_end = func_it
                    .peek()
                    .map(|(i, _)| i + 1)
                    .unwrap_or(end_table_index);
                mod_start..mod_end
            })
            .collect::<Vec<_>>();

        Ok(Self {
            table_entries,
            function_table_index,
            table_range_for_output_module,
        })
    }
}

#[derive(Debug)]
struct LateDataRange {
    input_range: Range<usize>,
    in_module: usize,
    data_align: usize, // power of 2
    in_module_offset: usize,
}

#[derive(Debug)]
enum DataSegmentEmitInfo {
    // Copy this segment from the input, either in all or a specific output module
    FromInputInAll,
    FromInputOnlyIn(usize),
    Ranges {
        // some reloc information
        base_address: usize,
        per_output_offset: HashMap<usize, usize>,
        // the output segment is formed by concatenating all these segment
        ranges: Vec<LateDataRange>,
        // symbol index -> (index in 'ranges', offset in range)
        range_lookup: HashMap<usize, (usize, usize)>,
        // we re-order ranges to put data with larger alignment up front (this saves padding bytes).
        // since indices are stored in the range_lookup map, we can't do this in-place and maintain a separate order here.
        range_emit_order: Vec<usize>,
    },
}

struct DataEmitInfo {
    per_segment: Vec<DataSegmentEmitInfo>,
}

impl DataEmitInfo {
    fn new(input_module: &InputModule, program_info: &SplitProgramInfo) -> Result<Self> {
        enum DataSegmentAnalysis {
            FromInputInAll,
            FromInputOnlyIn(usize),
            Ranges {
                ranges: Vec<LateDataRange>,
                // symbol -> (index in ranges, offset in range)
                range_lookup: HashMap<usize, (usize, usize)>,
                base_address: usize,
            },
        }
        let mut per_segment = input_module
            .data_segments
            .iter()
            .enumerate()
            .map(|(segment_idx, segment)| match &segment.kind {
                // [relocate data segments]
                // We duplicate all passive segments (there shouldn't be any except in multi-threading?)
                // because we don't have relocation to identify which function uses which passive data
                // for initialization. Hence we try to preserve indices as best as possible.
                DataKind::Passive => DataSegmentAnalysis::FromInputInAll,
                DataKind::Active { offset_expr, .. } => {
                    let segment_info = &input_module.reloc_info.segments[segment_idx];
                    if segment_info.flags.contains(SegmentFlags::TLS) {
                        return DataSegmentAnalysis::FromInputInAll;
                    }
                    let address = match offset_expr.get_operators_reader().read().unwrap() {
                        wasmparser::Operator::I32Const { value } => value as usize,
                        wasmparser::Operator::I64Const { value } => value as usize,
                        _ => return DataSegmentAnalysis::FromInputOnlyIn(0),
                    };
                    DataSegmentAnalysis::Ranges {
                        ranges: vec![],
                        range_lookup: HashMap::new(),
                        base_address: address,
                    }
                }
            })
            .collect::<Vec<_>>();

        // Now go through all data symbols
        for (module_index, (_, module)) in program_info.output_modules.iter().enumerate() {
            let mut included_symbols = module
                .included_symbols
                .iter()
                .filter_map(|symbol| {
                    let DepNode::DataSymbol(symbol_index) = *symbol else {
                        return None;
                    };
                    let SymbolInfo::Data {
                        symbol: Some(def_data),
                        ..
                    } = input_module.reloc_info.symbols[symbol_index]
                    else {
                        // Not sure how to emit an *undefined* data symbol
                        return Some(Err(anyhow!(
                            "Expected data symbol dep node to ref to defined data symbol"
                        )));
                    };
                    let segment_index = def_data.index as usize;
                    let DataSegmentAnalysis::Ranges { .. } = &per_segment[segment_index] else {
                        // Only relocate if in active range
                        return None;
                    };
                    Some(Ok((symbol_index, def_data)))
                })
                .collect::<Result<Vec<_>>>()?;
            included_symbols.sort_by_key(|(_, def_data)| (def_data.index, def_data.offset));
            for (symbol_index, def_data) in included_symbols {
                let segment_index = def_data.index as usize;
                let DataSegmentAnalysis::Ranges {
                    ranges,
                    range_lookup,
                    ..
                } = &mut per_segment[segment_index]
                else {
                    // filtered above. All passive ranges are put into the main module
                    unreachable!(
                        "data symbol in passive range should not have gotted included in this pass"
                    );
                };
                let data_len = def_data.size as usize;
                let data_offset = def_data.offset as usize;
                let data_range = data_offset..data_offset + data_len;
                let segment_align =
                    1usize << input_module.reloc_info.segments[segment_index].alignment;

                let mut data_align = segment_align;
                if data_offset != 0 {
                    // TODO: .isolate_least_significant_one()
                    data_align = data_align.min(1usize << data_offset.trailing_zeros());
                }
                if data_len != 0 {
                    data_align = data_align.min(1usize << data_len.trailing_zeros());
                }

                let mut has_merged = false;
                let range_idx = ranges.len();
                if let Some(back) = ranges.last_mut() {
                    let has_overlap =
                        back.in_module == module_index && data_offset < back.input_range.end;
                    if has_overlap {
                        // we sorted before ingesting, hence the existing range starts earlier
                        debug_assert!(
                            back.input_range.start <= data_offset,
                            "overlapping range goes backwards"
                        );
                        let range_offset = data_offset - back.input_range.start;
                        let range_idx = range_idx - 1; // back of ranges

                        has_merged = true;
                        // about alignment: we use the fact that llvm merged these symbols as proof that we too, do not have to worry about alignment
                        back.input_range.end = back.input_range.end.max(data_range.end);
                        range_lookup.insert(symbol_index, (range_idx, range_offset));
                    }
                }
                if !has_merged {
                    ranges.push(LateDataRange {
                        input_range: data_range,
                        in_module: module_index,
                        data_align,
                        in_module_offset: usize::MAX, // filled in later
                    });
                    range_lookup.insert(symbol_index, (range_idx, 0));
                }
            }
        }
        // finally transform them into output form
        let per_segment = per_segment
            .into_iter()
            .enumerate()
            .map(|(segment_index, segment)| match segment {
                DataSegmentAnalysis::FromInputInAll => DataSegmentEmitInfo::FromInputInAll,
                DataSegmentAnalysis::FromInputOnlyIn(module) => {
                    DataSegmentEmitInfo::FromInputOnlyIn(module)
                }
                DataSegmentAnalysis::Ranges {
                    mut ranges,
                    range_lookup,
                    base_address,
                } => {
                    let segment_alignment =
                        1usize << input_module.reloc_info.segments[segment_index].alignment;

                    let mut range_emit_order: Vec<_> = (0..ranges.len()).collect();
                    range_emit_order.sort_by_key(|&range_idx| {
                        let range = &ranges[range_idx];
                        (range.in_module, std::cmp::Reverse(range.data_align), range.input_range.start)
                    });
                    // reorder symbols per module
                    let mut per_module_size = HashMap::new();
                    for &range_idx in &range_emit_order {
                        let range = &mut ranges[range_idx];
                        let module_len = per_module_size.entry(range.in_module).or_insert(0);
                        let data_range = range.input_range.clone();
                        let data_offset = usize::next_multiple_of(*module_len, range.data_align);

                        // allocate it in that module
                        range.in_module_offset = data_offset;
                        *module_len = data_offset + data_range.len();
                    }

                    // check that range_lookup completely covers the (non-zero) data segment?
                    // Otherwise there is non-relocated data, which most likely indicates an error.
                    // There might be data symbols that are not included/depended upon anywhere though.
                    // Some gaps will exist, introduced by padding for alignment! This padding should be zeroed.
                    // So for the moment, don't bother with this sanity analysis.

                    // figure out module offsets
                    let mut per_module_size = per_module_size.into_iter().collect::<Vec<_>>();
                    per_module_size.sort_by_key(|&(m, _)| m);
                    let mut per_output_offset = HashMap::new();
                    let mut data_offset: usize = 0;
                    for &(module, module_size) in &per_module_size {
                        data_offset = data_offset.next_multiple_of(segment_alignment);
                        per_output_offset.insert(module, data_offset);
                        data_offset += module_size;
                    }
                    let segment_len = data_offset;

                    let ranges = DataSegmentEmitInfo::Ranges {
                        ranges,
                        base_address,
                        per_output_offset,
                        range_lookup,
                        range_emit_order,
                    };
                    // If we could move other active segments to different base addresses, this segment getting longer
                    // would not be a problem. Since we can't guarantee this at this point though, don't risk it.
                    // Overlapping segments *will* overwrite data!
                    if segment_len > input_module.data_segments[segment_index].data.len() {
                        let overlength = segment_len - input_module.data_segments[segment_index].data.len();
                        trace!("{ranges:?}");
                        warn!("Overlong segment {segment_index} by {overlength} after relocation, putting it in main module.");
                        DataSegmentEmitInfo::FromInputOnlyIn(0)
                    } else {
                        ranges
                    }
                }
            })
            .collect::<Vec<_>>();
        Ok(Self { per_segment })
    }
    fn find_relocated_address(
        &self,
        symbol_index: usize,
        data: &DefinedDataSymbol,
    ) -> Option<usize> {
        let segment_idx = data.index as usize;
        let DataSegmentEmitInfo::Ranges {
            ranges,
            base_address,
            per_output_offset,
            range_lookup,
            ..
        } = &self.per_segment[segment_idx]
        else {
            // If just copied, then its not relocated
            return None;
        };
        let &(range_index, offset_in_range) = range_lookup
            .get(&symbol_index)
            .expect("to find a data relocation index");
        let range = &ranges[range_index];
        let mut address = *base_address;
        address += per_output_offset[&range.in_module];
        address += range.in_module_offset;
        address += offset_in_range;
        Some(address)
    }
}

#[derive(Debug, Clone)]
pub struct OutputImport<'a> {
    pub module: Cow<'a, str>,
    pub name: Cow<'a, str>,
    pub ty: TypeRef,
}

#[derive(Debug, Clone)]
pub struct OutputExport<'a> {
    pub name: Cow<'a, str>,
    pub kind: ExternalKind,
    pub index: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Copy)]
enum OutputFunctionKind {
    Defined,
    IndirectStub,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct OutputFunction {
    input_func_id: InputFuncId,
    kind: OutputFunctionKind,
}

struct ModuleEmitState<'a> {
    input_module: &'a InputModule<'a>,
    emit_state: &'a EmitState<'a>,

    output_module_index: usize,
    output_module: wasm_encoder::Module,

    imports: Vec<OutputImport<'a>>,
    exports: Vec<OutputExport<'a>>,
    defined_functions: Vec<OutputFunction>,

    dep_to_local_index: HashMap<DepNode, usize>,
}

impl RelocTarget for ModuleEmitState<'_> {
    fn reloc_value(&self, reloc: RelocDetails<'_>) -> Result<Option<usize>> {
        match reloc {
            RelocDetails::TypeIndex { .. } => {
                // We don't relocate types, we just copy them over
                Ok(None)
            }
            RelocDetails::MemoryAddr(details) => {
                // [relocate data segments]
                let Some(symbol) = details.definition else {
                    return Ok(None);
                };
                Ok(self
                    .emit_state
                    .data_relocations
                    .find_relocated_address(details.symbol_index, symbol))
            }
            RelocDetails::TableIndex(details) => {
                let input_func_id = details.index;
                let index = self
                    .emit_state
                    .indirect_functions
                    .function_table_index
                    .get(&input_func_id)
                    .ok_or_else(|| {
                        anyhow!(
                            "Dependency analysis error: \
                             No indirect function table index \
                             for input function {input_func_id} \
                             referenced by relocation."
                        )
                    })?;
                Ok(Some(*index))
            }
            RelocDetails::RelTableIndex(_details) => {
                bail!("Unsupported relocation type: relative table index")
            }
            RelocDetails::FunctionIndex(details) => {
                let input_func_id = details.index;
                let Some(&output_func_id) = self
                    .dep_to_local_index
                    .get(&DepNode::Function(input_func_id))
                else {
                    bail!(
                        "Dependency analysis error: \
                             No output function for input function {input_func_id} \
                             referenced by relocation."
                    );
                };
                Ok(Some(output_func_id))
            }
            RelocDetails::TableNumber(details) => {
                if !self.is_main() && details.index != self.input_module.reloc_info.indirect_table {
                    bail!("Relocation of globals not supported in split modules.")
                }
                // TODO: check that table indices do not get confused by the generate logic below
                Ok(Some(0))
            }
            RelocDetails::GlobalIndex(details) => {
                if !self.is_main() && details.index != self.input_module.reloc_info.stack_pointer {
                    bail!("Relocation of globals not supported in split modules.")
                }
                // TODO: check that global indices do not get confused by the generate logic below
                Ok(Some(0))
            }
            RelocDetails::TagIndex(_details) => {
                if !self.is_main() {
                    bail!("Exception handling in split modules not supported yet")
                }
                Ok(None)
            }
        }
    }
}

impl<'a> ModuleEmitState<'a> {
    fn new(
        emit_state: &'a EmitState,
        output_module_index: usize,
        program_info: &'a crate::split_point::SplitProgramInfo,
    ) -> Self {
        let (_, output_module_info) = &program_info.output_modules[output_module_index];

        let mut num_func_imports = 0;
        let mut imports = vec![];
        let mut exports = vec![];
        let mut defined_functions = vec![];
        let mut dep_to_local_index = HashMap::new();

        for (func, func_import) in emit_state.input_module.imported_funcs.iter().enumerate() {
            if !output_module_info
                .included_symbols
                .contains(&DepNode::Function(func))
            {
                continue;
            }
            let import = &emit_state.input_module.imports[func_import.import_id];
            let local_func = num_func_imports;
            dep_to_local_index.insert(DepNode::Function(func), local_func);
            num_func_imports += 1;
            imports.push(OutputImport {
                module: import.module.into(),
                name: import.name.into(),
                ty: import.ty,
            });
        }

        for dep in &output_module_info.included_symbols {
            // define it
            match dep {
                DepNode::Function(func)
                    if *func >= emit_state.input_module.imported_funcs.len() =>
                {
                    let local_func = num_func_imports + defined_functions.len();
                    dep_to_local_index.insert(DepNode::Function(*func), local_func);
                    defined_functions.push(OutputFunction {
                        input_func_id: *func,
                        kind: OutputFunctionKind::Defined,
                    });
                }
                // already processed in the previous loop
                DepNode::Function(_) => {}
                // definition is implicit in the generate methods
                _ => {}
            }
        }
        if output_module_index != 0 {
            imports.push(emit_state.shared_import_for(&DepNode::Memory(0)));
        }

        let mut also_needs_indirect_table =
            !emit_state.indirect_functions.table_range_for_output_module[output_module_index]
                .is_empty();
        for used_shared in &output_module_info.used_shared_deps {
            if output_module_info.included_symbols.contains(used_shared) {
                continue;
            }
            match used_shared {
                DepNode::Function(func) => {
                    let local_func = num_func_imports + defined_functions.len();
                    dep_to_local_index.insert(DepNode::Function(*func), local_func);
                    also_needs_indirect_table = true; // Because we use it in the indirect stub
                    defined_functions.push(OutputFunction {
                        input_func_id: *func,
                        kind: OutputFunctionKind::IndirectStub,
                    });
                }
                dep @ DepNode::Global(_)
                | dep @ DepNode::Table(_)
                | dep @ DepNode::Tag(_)
                | dep @ DepNode::Memory(_) => {
                    imports.push(emit_state.shared_import_for(dep));
                }
                // memory addresses are already fixed
                DepNode::DataSymbol(_) => {}
            }
        }
        let ift_dep = DepNode::Table(emit_state.input_module.reloc_info.indirect_table);
        let defines_ift = output_module_info.included_symbols.contains(&ift_dep);
        let imports_ift = output_module_info.used_shared_deps.contains(&ift_dep);
        if also_needs_indirect_table && !defines_ift && !imports_ift {
            imports.push(emit_state.shared_import_for(&ift_dep));
        }

        // Map references to `import_func` to `export_func`.
        for (_, output_module) in program_info.output_modules.iter() {
            for split_point in output_module.split_points.iter() {
                if let Some(&output_func_id) =
                    dep_to_local_index.get(&DepNode::Function(split_point.export_func))
                {
                    dep_to_local_index
                        .insert(DepNode::Function(split_point.import_func), output_func_id);
                }
            }
        }

        // build exports
        let mut exported_dont_share = HashSet::new();
        for export in emit_state.input_module.exports.iter() {
            let dep = match export.kind {
                ExternalKind::Func => DepNode::Function(export.index as usize),
                ExternalKind::Table => DepNode::Table(export.index as usize),
                ExternalKind::Global => DepNode::Global(export.index as usize),
                ExternalKind::Tag => DepNode::Tag(export.index as usize),
                ExternalKind::Memory => continue,
            };
            if !output_module_info.included_symbols.contains(&dep) {
                continue;
            }
            let defined_id = match export.kind {
                ExternalKind::Func => {
                    let exported_fun = export.index as usize;
                    if program_info.split_point_exports.contains(&exported_fun) {
                        continue; // don't export split point functions, they are only shared
                    }
                    *dep_to_local_index
                        .get(&DepNode::Function(exported_fun))
                        .unwrap() as u32
                }
                _ => emit_state.shared_export_for(&dep).index,
            };
            exports.push(OutputExport {
                name: export.name.into(),
                kind: export.kind,
                index: defined_id,
            });
            exported_dont_share.insert(dep);
        }
        for dep in &output_module_info.included_symbols {
            if !program_info.shared_deps.contains(dep) || exported_dont_share.contains(dep) {
                continue;
            }
            // share it
            match dep {
                // functions are shared via indirection table
                DepNode::Function(_) => {}
                // export all globals, tables and tags
                dep @ DepNode::Global(_)
                | dep @ DepNode::Table(_)
                | dep @ DepNode::Tag(_)
                | dep @ DepNode::Memory(_) => {
                    exports.push(emit_state.shared_export_for(dep));
                }
                // memory symbols are currently all emitted in the main module
                DepNode::DataSymbol(_) => {}
            }
        }
        // share the indirect function table always
        if defines_ift
            && !exported_dont_share.contains(&ift_dep)
            && !program_info.shared_deps.contains(&ift_dep)
        {
            exports.push(emit_state.shared_export_for(&ift_dep));
        }

        Self {
            input_module: emit_state.input_module,
            output_module_index,
            emit_state,
            output_module: wasm_encoder::Module::new(),
            defined_functions,
            imports,
            exports,
            dep_to_local_index,
        }
    }

    fn is_main(&self) -> bool {
        self.output_module_index == 0
    }

    fn get_relocated_data(&self, range: Range<InputOffset>) -> Result<Vec<u8>> {
        RelocInfo::get_relocated_data(self.input_module, range, self)
    }

    fn generate(&mut self) -> Result<()> {
        // Encode type section
        self.generate_type_section()?;
        self.generate_import_section();
        self.generate_function_section();
        self.generate_table_section();
        self.generate_memory_section();
        self.generate_global_section();
        self.generate_export_section();
        self.generate_start_section();
        self.generate_element_section()?;
        self.generate_data_count_section();
        self.generate_code_section()?;
        self.generate_data_section()?;
        self.generate_wasm_bindgen_sections();
        self.generate_name_section()?;
        self.generate_target_features_section();
        self.generate_producers_section()?;
        Ok(())
    }

    fn generate_type_section(&mut self) -> Result<()> {
        // Simply copy all types.  Unneeded types may be pruned by `wasm-opt`.
        let mut section = wasm_encoder::TypeSection::new();
        for input_func_type in self.input_module.types.iter() {
            let output_func_type: wasm_encoder::FuncType =
                input_func_type.clone().try_into().unwrap();
            section.ty().function(
                output_func_type.params().iter().cloned(),
                output_func_type.results().iter().cloned(),
            );
        }
        self.output_module.section(&section);
        Ok(())
    }

    fn generate_import_section(&mut self) {
        let mut section = wasm_encoder::ImportSection::new();
        for imp in &self.imports {
            let module = if imp.module == magic_constants::PLACEHOLDER_IMPORT_MODULE {
                self.emit_state.link_module
            } else {
                &imp.module
            };
            section.import(module, &imp.name, EntityType::try_from(imp.ty).unwrap());
        }
        self.output_module.section(&section);
    }

    fn generate_function_section(&mut self) {
        let mut section = wasm_encoder::FunctionSection::new();
        for OutputFunction { input_func_id, .. } in self.defined_functions.iter() {
            section.function(self.input_module.func_type_id(*input_func_id) as u32);
        }
        self.output_module.section(&section);
    }

    fn generate_table_section(&mut self) {
        if !self.is_main() {
            return;
        }
        let mut section = wasm_encoder::TableSection::new();
        let mut defined_table_type = self.emit_state.get_indirect_function_table_type();
        defined_table_type.maximum = Some(defined_table_type.initial);
        section.table(defined_table_type.try_into().unwrap());
        self.output_module.section(&section);
    }

    fn generate_memory_section(&mut self) {
        if !self.is_main() || self.input_module.memories.is_empty() {
            return;
        }
        let mut section = wasm_encoder::MemorySection::new();
        for &memory in self.input_module.memories.iter() {
            section.memory(memory.into());
        }
        self.output_module.section(&section);
    }

    fn generate_global_section(&mut self) {
        if !self.is_main() {
            return;
        }
        let mut section = wasm_encoder::GlobalSection::new();
        for global in self.input_module.globals.iter() {
            section.global(
                global.ty.try_into().unwrap(),
                &global.init_expr.clone().try_into().unwrap(),
            );
        }
        self.output_module.section(&section);
    }

    fn generate_export_section(&mut self) {
        // shared functions are "exported" by placing them in the indirect_function_table.
        // shared globals are always exported from main
        let mut section = wasm_encoder::ExportSection::new();
        for exp in &self.exports {
            section.export(&exp.name, exp.kind.into(), exp.index);
        }
        self.output_module.section(&section);
    }

    fn generate_start_section(&mut self) {
        if !self.is_main() {
            return;
        }
        if let Some(input_start_func_id) = self.input_module.start {
            let output_func = self
                .dep_to_local_index
                .get(&DepNode::Function(input_start_func_id))
                .expect("Failed to map start function to output function index");
            self.output_module.section(&wasm_encoder::StartSection {
                function_index: *output_func as u32,
            });
        }
    }

    fn generate_element_section(&mut self) -> Result<()> {
        let indirect_range = &self
            .emit_state
            .indirect_functions
            .table_range_for_output_module[self.output_module_index];
        if indirect_range.is_empty() && !self.is_main() {
            return Ok(());
        }

        let indirect_table = Some(0);
        let mut section = wasm_encoder::ElementSection::new();
        let func_ids: Vec<u32> = indirect_range
            .clone()
            .map(|table_index| -> Result<u32> {
                let input_func_id =
                    self.emit_state.indirect_functions.table_entries[table_index - 1];
                let output_func_id = *self
                    .dep_to_local_index
                    .get(&DepNode::Function(input_func_id))
                    .ok_or_else(|| {
                        anyhow!(
                            "No output function corresponding to input function {input_func_id:?}"
                        )
                    })?;
                Ok(output_func_id as u32)
            })
            .collect::<Result<Vec<_>>>()?;
        section.segment(wasm_encoder::ElementSegment {
            mode: wasm_encoder::ElementMode::Active {
                table: indirect_table,
                offset: &wasm_encoder::ConstExpr::i32_const(indirect_range.start as i32),
            },
            elements: wasm_encoder::Elements::Functions(func_ids.into()),
        });
        // generate placeholders for all other indirection functions.
        // this is to "reserve" spots to keep safe from downstream processors.
        if self.is_main() {
            for (out_mod, range) in self
                .emit_state
                .indirect_functions
                .table_range_for_output_module
                .iter()
                .enumerate()
            {
                if out_mod == self.output_module_index {
                    continue;
                }
                let mut exprs = Vec::with_capacity(range.len());
                exprs.resize_with(range.len(), || {
                    wasm_encoder::ConstExpr::ref_null(wasm_encoder::HeapType::FUNC)
                });
                let elements = wasm_encoder::Elements::Expressions(
                    wasm_encoder::RefType::FUNCREF,
                    exprs.into(),
                );
                section.segment(wasm_encoder::ElementSegment {
                    mode: wasm_encoder::ElementMode::Active {
                        table: indirect_table,
                        offset: &wasm_encoder::ConstExpr::i32_const(range.start as i32),
                    },
                    elements,
                });
            }
        }
        self.output_module.section(&section);
        Ok(())
    }

    fn generate_data_count_section(&mut self) {
        let section = wasm_encoder::DataCountSection {
            count: self.input_module.data_segments.len() as u32,
        };
        self.output_module.section(&section);
    }

    fn generate_indirect_stub(
        &self,
        indirect_index: usize,
        type_id: usize,
    ) -> wasm_encoder::Function {
        let func_type = &self.input_module.types[type_id];
        let mut func = wasm_encoder::Function::new([]);
        for (param_i, _param_type) in func_type.params().iter().enumerate() {
            func.instruction(&wasm_encoder::Instruction::LocalGet(param_i as u32));
        }
        func.instruction(&wasm_encoder::Instruction::I32Const(indirect_index as i32));
        func.instruction(&wasm_encoder::Instruction::CallIndirect {
            type_index: type_id as u32,
            table_index: 0,
        });
        func.instruction(&wasm_encoder::Instruction::End);
        func
    }

    fn generate_code_section(&mut self) -> Result<()> {
        let mut section = wasm_encoder::CodeSection::new();
        for output_func in self.defined_functions.iter() {
            match output_func.kind {
                OutputFunctionKind::Defined => {
                    let input_func = &self.input_module.defined_funcs
                        [output_func.input_func_id - self.input_module.imported_funcs.len()];
                    let relocated_def = self
                        .get_relocated_data(input_func.body.range())
                        .with_context(|| {
                            format!(
                                "when emitted definition of func[{}] in module {}",
                                output_func.input_func_id, self.output_module_index,
                            )
                        })?;
                    section.raw(&relocated_def);
                }
                OutputFunctionKind::IndirectStub => {
                    let indirect_index = self
                        .emit_state
                        .indirect_functions
                        .function_table_index
                        .get(&output_func.input_func_id)
                        .unwrap();
                    let function = self.generate_indirect_stub(
                        *indirect_index,
                        self.input_module.func_type_id(output_func.input_func_id),
                    );
                    section.function(&function);
                }
            }
        }
        self.output_module.section(&section);
        Ok(())
    }

    fn get_relocated_segment_data(&self, data: &Data<'_>) -> Result<Vec<u8>> {
        // Note: `data.range` includes the segment header.
        let range_end = data.range.end;
        let range_start = range_end - data.data.len();
        self.get_relocated_data(range_start..range_end)
    }

    fn generate_data_section(&mut self) -> Result<()> {
        let data_reloc = &self.emit_state.data_relocations;
        let mut section = wasm_encoder::DataSection::new();
        for (segment_idx, segment) in data_reloc.per_segment.iter().enumerate() {
            let input_data = &self.input_module.data_segments[segment_idx];
            let input_range_end = input_data.range.end;
            let input_range_start = input_range_end - input_data.data.len();

            let mut data: Vec<u8>;
            let addr_offset: Option<usize>;
            match segment {
                DataSegmentEmitInfo::FromInputInAll => {
                    addr_offset = None;
                    data = self.get_relocated_segment_data(input_data)?;
                }
                DataSegmentEmitInfo::FromInputOnlyIn(module)
                    if *module == self.output_module_index =>
                {
                    addr_offset = None;
                    data = self.get_relocated_segment_data(input_data)?;
                }
                DataSegmentEmitInfo::FromInputOnlyIn(_) => {
                    addr_offset = None;
                    data = vec![]; // no data, but emit the module to not shift data indices
                }
                DataSegmentEmitInfo::Ranges {
                    ranges,
                    range_emit_order,
                    per_output_offset,
                    base_address,
                    ..
                } => {
                    if let Some(module_offset) = per_output_offset.get(&self.output_module_index) {
                        addr_offset = Some(base_address + *module_offset);
                    } else {
                        addr_offset = None;
                    }
                    data = vec![];
                    for &range_idx in range_emit_order {
                        let range = &ranges[range_idx];
                        if range.in_module != self.output_module_index {
                            continue;
                        }
                        let data_range = &range.input_range;
                        let input_range = (input_range_start + data_range.start)
                            ..(input_range_start + data_range.end);
                        data.resize(range.in_module_offset, 0); // pad with zeroes
                        data.extend(self.get_relocated_data(input_range)?);
                    }
                }
            }
            match input_data.kind {
                DataKind::Passive => {
                    section.passive(data);
                }
                DataKind::Active {
                    memory_index,
                    ref offset_expr,
                } => {
                    let offset = match addr_offset {
                        None => offset_expr.clone().try_into().unwrap(),
                        Some(module_offset) => {
                            if module_offset <= i32::MAX as usize {
                                wasm_encoder::ConstExpr::i32_const(module_offset as i32)
                            } else {
                                wasm_encoder::ConstExpr::i64_const(module_offset as i64)
                            }
                        }
                    };
                    section.active(memory_index, &offset, data);
                }
            }
        }
        self.output_module.section(&section);
        Ok(())
    }

    fn generate_name_section(&mut self) -> Result<()> {
        fn convert_name_map(parser_map: &wasmparser::NameMap<'_>) -> Result<wasm_encoder::NameMap> {
            let mut encoder_map = wasm_encoder::NameMap::new();
            for r in parser_map.clone().into_iter() {
                let naming = r?;
                encoder_map.append(naming.index, naming.name);
            }
            Ok(encoder_map)
        }

        fn convert_name_hash_map(map: &HashMap<usize, &str>) -> wasm_encoder::NameMap {
            let mut encoder_map = wasm_encoder::NameMap::new();
            let mut names = map.iter().map(|(i, name)| (*i, *name)).collect::<Vec<_>>();
            names.sort();
            for (i, name) in names {
                encoder_map.append(i as u32, name);
            }
            encoder_map
        }
        let mut section = wasm_encoder::NameSection::new();
        // Function names
        {
            let mut name_map = wasm_encoder::NameMap::new();
            let mut locals_map = wasm_encoder::IndirectNameMap::new();
            let mut labels_map = wasm_encoder::IndirectNameMap::new();
            for OutputFunction { input_func_id, .. } in &self.defined_functions {
                let Some(&output_func_id) = self
                    .dep_to_local_index
                    .get(&DepNode::Function(*input_func_id))
                else {
                    continue;
                };
                if let Some(name) = self.input_module.names.functions.get(input_func_id) {
                    name_map.append(output_func_id as u32, name);
                }
                if let Some(name_map) = self.input_module.names.locals.get(input_func_id) {
                    locals_map.append(output_func_id as u32, &convert_name_map(name_map)?);
                }
                if let Some(name_map) = self.input_module.names.labels.get(input_func_id) {
                    labels_map.append(output_func_id as u32, &convert_name_map(name_map)?);
                }
            }
            section.functions(&name_map);
            section.locals(&locals_map);
            section.labels(&labels_map);
        }
        section.types(&convert_name_hash_map(&self.input_module.names.types));
        section.tables(&convert_name_hash_map(&self.input_module.names.tables));
        section.memories(&convert_name_hash_map(&self.input_module.names.memories));
        section.globals(&convert_name_hash_map(&self.input_module.names.globals));
        // elements
        section.data(&convert_name_hash_map(
            &self.input_module.names.data_segments,
        ));
        // tag
        // fields
        // tags
        self.output_module.section(&section);
        Ok(())
        // Type names
    }

    fn generate_wasm_bindgen_sections(&mut self) {
        for custom in self.input_module.custom_sections.iter() {
            if self.is_main() && custom.name == "__wasm_bindgen_unstable" {
                self.output_module.section(&wasm_encoder::CustomSection {
                    name: custom.name.into(),
                    data: custom.data.into(),
                });
            }
        }
    }

    fn generate_target_features_section(&mut self) {
        for custom in self.input_module.custom_sections.iter() {
            if custom.name == "target_features" {
                // Another wasm-bindgen hack: To make sure reference-types is not detected, replace the feature string :)
                let mut data: Vec<u8> = custom.data.into();
                if self.is_main() {
                    // 0x0f is the length of the following string
                    let needle = b"+\x0freference-types";
                    if let Some(pos) = data.windows(needle.len()).position(|feat| feat == needle) {
                        data[pos..pos + needle.len()].copy_from_slice(b"+\x0fREFERENCE-TYPES");
                    }
                }

                self.output_module.section(&wasm_encoder::CustomSection {
                    name: custom.name.into(),
                    data: data.into(),
                });
            }
        }
    }

    fn generate_producers_section(&mut self) -> Result<()> {
        let mut producers = ProducersSection::new();
        let mut produced_by = ProducersField::new();
        const PRODUCERS_NAME: &str = "producers";
        const PROCESSED_BY_FIELD_NAME: &str = "processed-by";

        if self.is_main() {
            // copy the section from input wasm, but insert ourselves
            if let Some(input_producers) = self
                .input_module
                .custom_sections
                .iter()
                .find(|section| section.name == PRODUCERS_NAME)
            {
                let fields = ProducersSectionReader::new(BinaryReader::new(
                    input_producers.data,
                    input_producers.data_offset,
                ))?;
                for input_field in fields.into_iter() {
                    let input_field = input_field?;
                    let mut field = ProducersField::new();
                    for entry in input_field.values.into_iter() {
                        let entry = entry?;
                        field.value(entry.name, entry.version);
                    }
                    if input_field.name == PROCESSED_BY_FIELD_NAME {
                        produced_by = field;
                    } else {
                        producers.field(input_field.name, &field);
                    }
                }
            }
        }
        produced_by.value("wasm_split_cli_support", env!("CARGO_PKG_VERSION"));
        producers.field(PROCESSED_BY_FIELD_NAME, &produced_by);
        self.output_module.section(&producers);
        Ok(())
    }
}

pub fn emit_modules(
    module: &InputModule,
    program_info: &SplitProgramInfo,
    link_module: &str,
    mut emit_fn: impl FnMut(usize, &[u8]) -> Result<()>,
) -> Result<()> {
    let emit_state = EmitState::new(module, program_info, link_module)?;

    for output_module_index in 0..program_info.output_modules.len() {
        let mut emit_state = ModuleEmitState::new(&emit_state, output_module_index, program_info);
        let identifier = &program_info.output_modules[output_module_index].0;

        emit_state
            .generate()
            .with_context(|| format!("Error generating {:?}", identifier))?;

        emit_fn(output_module_index, emit_state.output_module.as_slice())
            .with_context(|| format!("Error emitting {:?}", identifier))?;
    }

    Ok(())
}
