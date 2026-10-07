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
    split_point::{
        data::{DataEmitInfo, DataSegmentEmitInfo, Fragment},
        SplitModuleIdentifier, SplitProgramInfo, MAIN_MODULE,
    },
    tracing_support::perf_span,
    util::wasm_data_start,
};
use eyre::{bail, Context, Result};
use wasm_encoder::{reencode::Reencode, ConstExpr, EntityType, ProducersField, ProducersSection};
use wasmparser::{Data, DataKind, ExternalKind, Operator, RelocationType, TypeRef};

pub(crate) struct EmitState<'a> {
    input_options: &'a crate::Options<'a>,
    input_module: &'a InputModule<'a>,
    link_module: &'a str,
    // info about shared usage
    indirect_functions: IndirectFunctionEmitInfo,
    shared_names: HashMap<DepNode, Cow<'a, str>>,
    no_reloc_stubs: &'a HashSet<InputFuncId>,
    canary_import_name: &'a str,
    data_relocations: &'a DataEmitInfo,
}

impl<'a> EmitState<'a> {
    pub(crate) fn new(
        input_options: &'a crate::Options<'a>,
        module: &'a InputModule<'a>,
        program_info: &'a SplitProgramInfo,
        link_module: &'a str,
        no_reloc_stubs: &'a HashSet<InputFuncId>,
    ) -> Result<Self> {
        let indirect_functions = IndirectFunctionEmitInfo::new(module, program_info)?;

        let mut shared_names = HashMap::new();
        // We potentially overwrite the mapping later on again, but that's okay.
        // We could also check the reloc section for better names.
        let mut shared_by_import = program_info
            .shared_deps
            .iter()
            .filter(|dep| !matches!(dep, DepNode::Function(_) | DepNode::DataSymbol(_)))
            .collect::<Vec<_>>();
        shared_by_import.sort();
        for (unique_id, dep) in shared_by_import.into_iter().enumerate() {
            let chosen_name = Cow::Owned(format!("__wasm_split_shared{unique_id}"));
            shared_names.insert(*dep, chosen_name);
        }

        shared_names.insert(
            module.indirect_function_table(),
            Cow::Borrowed("__indirect_function_table"),
        );
        shared_names.insert(module.main_memory(), Cow::Borrowed("memory"));

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

        let unique_names = shared_names
            .values()
            .map(|name| name.as_ref())
            .collect::<HashSet<_>>();
        if unique_names.len() != shared_names.len() {
            bail!(
                "Failed to generate unique names for some exports. \
                This is a bug in wasm-split, please report this as an issue."
            );
        }

        Ok(EmitState {
            input_options,
            input_module: module,
            link_module,
            indirect_functions,
            shared_names,
            no_reloc_stubs,
            canary_import_name: program_info.canary_export_name(),
            data_relocations: &program_info.data_relocations,
        })
    }

    pub(crate) fn input(&self) -> &'a InputModule<'a> {
        self.input_module
    }
    pub(crate) fn input_options(&self) -> &'a crate::Options<'a> {
        self.input_options
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
    pub(crate) fn name_for(&self, dep: &DepNode) -> Cow<'a, str> {
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

#[derive(Default)]
struct IndirectFunctionEmitInfo {
    table_entries: Vec<InputFuncId>,
    function_table_index: HashMap<InputFuncId, usize>,
    table_range_for_output_module: Vec<Range<usize>>,
}

impl std::fmt::Debug for IndirectFunctionEmitInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = self.function_table_index;
        f.debug_struct("IndirectFunctionEmitInfo")
            .field("table_entries", &self.table_entries)
            // Omit this field since it's derived from table_entries
            // .field("function_table_index", &self.function_table_index)
            .field(
                "table_range_for_output_module",
                &self.table_range_for_output_module,
            )
            .finish()
    }
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
        for split_point in program_info.split_points.iter() {
            indirect_functions.remove(&split_point.import_func);
        }

        let module_for_func = |func_id| {
            *program_info
                .symbol_output_module
                .get(&DepNode::Function(func_id))
                .unwrap_or_else(|| panic!("No module for indirect function {func_id}"))
        };

        let mut table_entries: Vec<_> = indirect_functions.into_iter().collect();
        // all keys are unique, hence this sort is actually stable
        table_entries.sort_unstable_by_key(|&func_id| (module_for_func(func_id), func_id));
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

type ModuleFuncId = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum OutputFunction {
    DefinedFromInput {
        input_func_id: InputFuncId,
        local_index: ModuleFuncId,
    },
    IndirectCallShim {
        input_func_id: InputFuncId,
        shim_index: ModuleFuncId,
    },
    LocalShim {
        input_func_id: InputFuncId,
        shim_index: ModuleFuncId,
    },
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
    local_shims: HashMap<InputFuncId, ModuleFuncId>,

    function_header_len: usize,
    function_offset_hint: HashMap<ModuleFuncId, usize>,
}

impl RelocTarget for ModuleEmitState<'_> {
    fn reloc_value(&self, reloc: RelocDetails<'_>) -> Result<Option<u64>> {
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
                let Ok(address) = self
                    .emit_state
                    .data_relocations
                    .find_relocated_address(details.symbol_index, symbol)
                else {
                    bail!(
                        "Dependency analysis error: \
                        No output address for data symbol {symbol:?} \
                        referenced by relocation."
                    );
                };
                Ok(address)
            }
            RelocDetails::TableIndex(details) => {
                let input_func_id = details.index;
                let index = self
                    .emit_state
                    .indirect_functions
                    .function_table_index
                    .get(&input_func_id);
                // in some cases, we fallback to emitting the whole module's data in the main module
                // in these cases, some data symbols reference functions that we found to be unreachable
                // these do not get an index in the emitted function table. We rewrite them to the
                // index 0, which will trap when executed.
                // no other relocations are expected against data segments.
                let index = index.cloned().unwrap_or(0);
                // TODO: we could assert that we are indeed relocating a data segment here.
                // an improved analysis of data segments could perhaps avoid this case entirely
                // and restore the assertion that the referenced function is reachable.
                Ok(Some(index as u64))
            }
            RelocDetails::RelTableIndex(_details) => {
                bail!("Unsupported relocation type: relative table index");
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
                Ok(Some(output_func_id as u64))
            }
            RelocDetails::TableNumber(details) => {
                if !self.is_main() && details.index != self.input_module.reloc_info.indirect_table {
                    bail!("Relocation of tables not supported in split modules.");
                }
                // TODO: check that table indices do not get confused by the generate logic below
                Ok(Some(0))
            }
            RelocDetails::GlobalIndex(details) => {
                if !self.is_main() && details.index != self.input_module.reloc_info.stack_pointer {
                    bail!("Relocation of globals not supported in split modules.");
                }
                // TODO: check that global indices do not get confused by the generate logic below
                Ok(Some(0))
            }
            RelocDetails::TagIndex(_details) => {
                if !self.is_main() {
                    bail!("Exception handling in split modules not supported yet");
                }
                Ok(None)
            }
            _ => {
                bail!("unexpected relocation {reloc:?} in code/data section");
            }
        }
    }
}

mod dwarf;

impl<'a> ModuleEmitState<'a> {
    fn new(
        emit_state: &'a EmitState,
        output_module_index: usize,
        program_info: &'a crate::split_point::SplitProgramInfo,
    ) -> Self {
        let perf_span = perf_span!(
            "build module emit state",
            module_index = output_module_index
        );
        let _perf_span = perf_span.enter();
        let (_, output_module_info) = &program_info.output_modules[output_module_index];

        let mut num_func_imports = 0;
        let mut imports = vec![];
        let mut exports = vec![];
        let mut defined_functions = vec![];
        let mut dep_to_local_index = HashMap::new();
        let mut local_shims = HashMap::new();

        for (func, func_import) in emit_state.input_module.imported_funcs.iter().enumerate() {
            if !output_module_info
                .included_symbols
                .contains(&DepNode::Function(func))
            {
                continue;
            }
            debug_assert!(
                output_module_index == MAIN_MODULE,
                "expected a function import to happen in the main module"
            );
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

        let mut included_symbols = output_module_info
            .included_symbols
            .iter()
            .cloned()
            .collect::<Vec<_>>();
        included_symbols.sort();
        for dep in &included_symbols {
            if let &dep @ DepNode::Function(input_func) = dep {
                let is_import = input_func < emit_state.input_module.imported_funcs.len();
                if !is_import {
                    let local_func = num_func_imports + defined_functions.len();
                    defined_functions.push(OutputFunction::DefinedFromInput {
                        input_func_id: input_func,
                        local_index: local_func,
                    });
                    dep_to_local_index.insert(dep, local_func);
                }
                if (is_import && program_info.shared_deps.contains(&dep))
                    || program_info.needs_shim_in_main.contains(&input_func)
                {
                    let local_func = num_func_imports + defined_functions.len();
                    defined_functions.push(OutputFunction::LocalShim {
                        input_func_id: input_func,
                        shim_index: local_func,
                    });
                    local_shims.insert(input_func, local_func);
                    tracing::trace!("Adding shim for {input_func} -> {local_func}");
                }
            }
            // for other types of symbols, the definition is implicit
        }

        let mut also_needs_indirect_table =
            !emit_state.indirect_functions.table_range_for_output_module[output_module_index]
                .is_empty();
        let mut used_shared_deps = output_module_info
            .used_shared_deps
            .iter()
            .collect::<Vec<_>>();
        used_shared_deps.sort();
        for used_shared in used_shared_deps {
            if output_module_info.included_symbols.contains(used_shared) {
                continue;
            }
            match used_shared {
                DepNode::Function(func) => {
                    let local_func = num_func_imports + defined_functions.len();
                    defined_functions.push(OutputFunction::IndirectCallShim {
                        input_func_id: *func,
                        shim_index: local_func,
                    });
                    dep_to_local_index.insert(DepNode::Function(*func), local_func);
                    also_needs_indirect_table = true; // Because we use it in the indirect stub
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
        let ift_dep = emit_state.input_module.indirect_function_table();
        let defines_ift = output_module_info.included_symbols.contains(&ift_dep);
        let imports_ift = output_module_info.used_shared_deps.contains(&ift_dep);
        if !imports_ift && also_needs_indirect_table && !defines_ift {
            if emit_state.input_options.strict_tests {
                panic!("module should import indirect function table because it needs it");
            }
            // we shouldn't hit this, but if we accidentally do, it's easy to fix
            imports.push(emit_state.shared_import_for(&ift_dep));
        }

        // Map references to `import_func` to `export_func`.
        for split_point in program_info.split_points.iter() {
            if let Some(&output_func_id) =
                dep_to_local_index.get(&DepNode::Function(split_point.export_func))
            {
                dep_to_local_index
                    .insert(DepNode::Function(split_point.import_func), output_func_id);
            }
        }

        // build exports
        let mut exported_dont_share = HashSet::new();
        for export in emit_state.input_module.exports.iter() {
            let dep = match export.kind {
                ExternalKind::Func | ExternalKind::FuncExact => {
                    DepNode::Function(export.index as usize)
                }
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
        for dep in &included_symbols {
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
            emit_state,
            output_module_index,
            output_module: wasm_encoder::Module::new(),
            defined_functions,
            imports,
            exports,
            dep_to_local_index,
            local_shims,
            function_header_len: 0,
            function_offset_hint: Default::default(),
        }
    }

    fn is_main(&self) -> bool {
        self.output_module_index == MAIN_MODULE
    }

    fn get_relocated_data(
        &self,
        range: Range<InputOffset>,
        data_address: Option<u64>,
    ) -> Result<Vec<u8>> {
        RelocInfo::get_relocated_data(self.input_module, range, self, data_address)
    }

    fn generate(&mut self) -> Result<()> {
        let perf_span = perf_span!("generate wasm", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        // Encode sections. These must occur in order
        // most custom sections at the end. The wasm standard doesn't specify a (partial) order, but llvm does
        // reference: https://github.com/llvm/llvm-project/blob/b5fa9eee6798b678fc7cb5f2b42a977932b708f9/llvm/lib/Object/WasmObjectFile.cpp#L2212-L2220
        // type < dylink
        // data < linking
        // linking < reloc, name
        // name < producers
        // producers < target_features
        // we don't emit dylink, linking, reloc at this point.
        self.generate_type_section()?;
        self.generate_import_section();
        self.generate_function_section();
        self.generate_table_section();
        self.generate_memory_section();
        self.generate_global_section()?;
        self.generate_export_section();
        self.generate_start_section();
        self.generate_element_section()?;
        self.generate_data_count_section();
        self.generate_code_section()?;
        self.generate_data_section()?;
        // our chosen order thus is: name -> producers -> target_features
        self.generate_name_section()?;
        self.generate_producers_section()?;
        self.generate_target_features_section();
        self.generate_debug_sections()?;
        self.generate_wasm_bindgen_sections();
        Ok(())
    }

    fn generate_type_section(&mut self) -> Result<()> {
        let perf_span = perf_span!("type section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
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
        let perf_span = perf_span!("import section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        let mut section = wasm_encoder::ImportSection::new();
        for imp in &self.imports {
            let module = if imp.module == magic_constants::PLACEHOLDER_IMPORT_MODULE {
                self.emit_state.link_module
            } else {
                &imp.module
            };
            section.import(module, &imp.name, EntityType::try_from(imp.ty).unwrap());
        }
        if !self.is_main() && self.emit_state.input_module.options.debug_assertions {
            section.import(
                "__wasm_split",
                self.emit_state.canary_import_name,
                EntityType::Global(wasm_encoder::GlobalType {
                    val_type: wasm_encoder::ValType::I32,
                    mutable: false,
                    shared: false,
                }),
            );
        }
        self.output_module.section(&section);
    }

    fn generate_function_section(&mut self) {
        let perf_span = perf_span!("function section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        let mut section = wasm_encoder::FunctionSection::new();
        for defined_func in self.defined_functions.iter() {
            let func_ty = match defined_func {
                OutputFunction::DefinedFromInput { input_func_id, .. }
                | OutputFunction::LocalShim { input_func_id, .. }
                | OutputFunction::IndirectCallShim { input_func_id, .. } => {
                    self.input_module.func_type_id(*input_func_id)
                }
            };
            section.function(func_ty as u32);
        }
        self.output_module.section(&section);
    }

    fn generate_table_section(&mut self) {
        let perf_span = perf_span!("table section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
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

    fn generate_global_section(&mut self) -> Result<()> {
        let perf_span = perf_span!("global section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        if !self.is_main() {
            return Ok(());
        }
        let mut section = wasm_encoder::GlobalSection::new();
        for (global_idx, global) in self.input_module.globals.iter().enumerate() {
            let global_expr = global.init_expr.clone();
            let mut init_expr = global_expr.clone().try_into().unwrap();
            if let Some(&symbol) = self
                .emit_state
                .input_module
                .reloc_info
                .symbol_as_global
                .get(&global_idx)
            {
                let (reloc_type, conv_reloc_value): (RelocationType, fn(u64) -> ConstExpr) =
                    match global_expr.get_operators_reader().read() {
                        Ok(Operator::I32Const { value: _ }) => {
                            (RelocationType::MemoryAddrI32, |val: u64| {
                                ConstExpr::i32_const(val.try_into().unwrap())
                            })
                        }
                        Ok(Operator::I64Const { value: _ }) => {
                            (RelocationType::MemoryAddrI64, |val: u64| {
                                ConstExpr::i64_const(val.try_into().unwrap())
                            })
                        }
                        _ => {
                            bail!(
                                "Expected a i32/i64.const expression for an exported data symbol"
                            );
                        }
                    };
                let fake_reloc = wasmparser::RelocationEntry {
                    ty: reloc_type,
                    offset: 0,
                    index: symbol as u32,
                    addend: 0,
                };
                let details = self
                    .emit_state
                    .input_module
                    .reloc_info
                    .expand_relocation(&fake_reloc)
                    .expect("fake reloc to look good");
                if let Some(resolved_addr) =
                    self.reloc_value(details).expect("to resolve an address")
                {
                    init_expr = conv_reloc_value(resolved_addr);
                }
            }
            section.global(global.ty.try_into().unwrap(), &init_expr);
        }
        self.output_module.section(&section);
        Ok(())
    }

    fn generate_export_section(&mut self) {
        let perf_span = perf_span!("export section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        // shared functions are "exported" by placing them in the indirect_function_table.
        // shared globals are always exported from main
        let mut section = wasm_encoder::ExportSection::new();
        for exp in &self.exports {
            section.export(&exp.name, exp.kind.into(), exp.index);
        }
        self.output_module.section(&section);
    }

    fn generate_start_section(&mut self) {
        let perf_span = perf_span!("start section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
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
        let perf_span = perf_span!("element section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();

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
                // If we have a local shim, we insert that into the indirect function table instead
                // of the import directly.
                // The reason is that wasm-bindgen will rewrite the signature of externref imports.
                // Having such a function in the IFT would lead to mismatching signatures. The call
                // in the shim will get rewritten correctly (in the main module).
                // Note: for calls (and other reloc targets), we instead prefer not going through the shim.
                let shim_func_id = self.local_shims.get(&input_func_id);
                let output_func_id = shim_func_id.or_else(|| {
                    self.dep_to_local_index
                        .get(&DepNode::Function(input_func_id))
                });
                let Some(&output_func_id) = output_func_id else {
                    bail!("No output function corresponding to input function {input_func_id:?}");
                };
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
        let data_section_count = self.input_module.data_segments.len()
            + self
                .emit_state
                .data_relocations
                .extra_fragments(self.output_module_index)
                .len();
        let section = wasm_encoder::DataCountSection {
            count: data_section_count
                .try_into()
                .expect("unexpectedly large number of data sections"),
        };
        self.output_module.section(&section);
    }

    fn generate_indirect_shim(
        &self,
        indirect_index: usize,
        type_id: usize,
    ) -> wasm_encoder::Function {
        let func_type = &self.input_module.types[type_id];
        let mut func = wasm_encoder::Function::new([]);
        let mut sink = func.instructions();
        for (param_i, _param_type) in func_type.params().iter().enumerate() {
            sink.local_get(param_i as u32);
        }
        sink.i32_const(indirect_index as i32);
        // TODO: can optionally use return_call_indirect for tail call optimizations
        sink.call_indirect(0, type_id as u32);
        sink.end();
        func
    }

    fn generate_local_shim(
        &self,
        imported_func_id: usize,
        type_id: usize,
    ) -> wasm_encoder::Function {
        let func_type = &self.input_module.types[type_id];
        let mut func = wasm_encoder::Function::new([]);
        let mut sink = func.instructions();
        for (param_i, _param_type) in func_type.params().iter().enumerate() {
            sink.local_get(param_i as u32);
        }
        // TODO: can optionally use return_call for tail call optimizations
        sink.call(imported_func_id as u32);
        sink.end();
        func
    }

    fn generate_code_section(&mut self) -> Result<()> {
        let perf_span = perf_span!("code section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        tracing::debug!(
            module = self.output_module_index,
            defined_count = self.defined_functions.len(),
            "emitting code section",
        );
        let mut section = wasm_encoder::CodeSection::new();
        for output_func in self.defined_functions.iter() {
            match *output_func {
                OutputFunction::DefinedFromInput { input_func_id, .. }
                    if self.emit_state.no_reloc_stubs.contains(&input_func_id) =>
                {
                    let body = self.input_module.defined_funcs
                        [input_func_id - self.input_module.imported_funcs.len()]
                    .body
                    .clone();

                    FuncReencoder {
                        dep_to_local_index: &self.dep_to_local_index,
                        func_id: input_func_id,
                    }
                    .parse_function_body(&mut section, body)
                    .with_context(|| format!("re-encoding no-reloc func {}", input_func_id))?;
                }
                OutputFunction::DefinedFromInput {
                    input_func_id,
                    local_index,
                } => {
                    let input_func = &self.input_module.defined_funcs
                        [input_func_id - self.input_module.imported_funcs.len()];
                    let relocated_def = self
                        .get_relocated_data(input_func.body.range(), None)
                        .with_context(|| {
                            format!(
                                "when emitted definition of func[{}] in module {}",
                                input_func_id, self.output_module_index,
                            )
                        })?;
                    section.raw(&relocated_def);
                    let offset_after = section.byte_len();
                    let func_offset = offset_after - relocated_def.len();
                    tracing::trace!("FunctionOffset[{}] = {}", local_index, func_offset);
                    self.function_offset_hint.insert(local_index, func_offset);
                }
                OutputFunction::IndirectCallShim { input_func_id, .. } => {
                    let indirect_index = self
                        .emit_state
                        .indirect_functions
                        .function_table_index
                        .get(&input_func_id)
                        .unwrap();
                    let function = self.generate_indirect_shim(
                        *indirect_index,
                        self.input_module.func_type_id(input_func_id),
                    );
                    section.function(&function);
                }
                OutputFunction::LocalShim { input_func_id, .. } => {
                    let emitted_func_id = *self
                        .dep_to_local_index
                        .get(&DepNode::Function(input_func_id))
                        .expect("input to have a name in this output module");
                    let type_id = self.input_module.func_type_id(input_func_id);
                    let function = self.generate_local_shim(emitted_func_id, type_id);
                    section.function(&function);
                }
            }
        }
        // We need to adjust the offsets by the header of the section.
        // The section gets written as
        // struct {
        //     id: const(10u8),
        //     len: uleb(_),
        //     // ---- offsets must be relative to this address
        //     count: uleb(function_count),
        //     // ---- offsets above are relative to this address, since we don't predict function_count
        //     bytes: [u8; section.byte_len()],
        // }
        let offset_before_section = self.output_module.len();
        let content_len = section.byte_len();
        self.output_module.section(&section);
        let offset_before_content = self.output_module.len() - content_len;

        let mut target_offset = offset_before_section;
        target_offset += 1; // skip id byte (constant 0xA)
        target_offset += encoded_uleb_len(&self.output_module.as_slice()[target_offset..]);
        self.function_header_len = offset_before_content - target_offset;
        tracing::trace!("function_header_len = {}", self.function_header_len);
        Ok(())
    }

    fn get_relocated_segment_data(&self, segment_idx: usize, data: &Data<'_>) -> Result<Vec<u8>> {
        // Note: `data.range` includes the segment header.
        let range_end = data.range.end;
        let range_start = wasm_data_start(data);
        // unchanged from the input in this case
        let segment_address = self
            .input_module
            .reloc_info
            .get_segment_data_address(segment_idx);
        self.get_relocated_data(range_start..range_end, segment_address)
    }

    /// The data of `fragment`, with the relocations inside applied.
    fn fragment_data(&self, segment_idx: usize, fragment: &Fragment) -> Result<Vec<u8>> {
        let DataSegmentEmitInfo::Ranges { ranges, layout, .. } =
            &self.emit_state.data_relocations.per_segment[segment_idx]
        else {
            unreachable!("fragments only exist for relocated segments");
        };
        let segment_address = self
            .input_module
            .reloc_info
            .get_segment_data_address(segment_idx);
        let input_range_start = wasm_data_start(&self.input_module.data_segments[segment_idx]);
        let mut data = vec![];
        for &range_idx in &layout.emit_order[fragment.ranges.clone()] {
            let range = &ranges[range_idx];
            let input_range = (input_range_start + range.input_range.start)
                ..(input_range_start + range.input_range.end);
            data.resize((range.segment_offset - fragment.offset) as usize, 0); // pad with zeroes
            let adjusted_address = segment_address.map(|addr| {
                addr.wrapping_add(range.segment_offset)
                    .wrapping_sub(range.input_range.start)
            });
            data.extend(self.get_relocated_data(input_range, adjusted_address)?);
        }
        Ok(data)
    }

    fn generate_data_section(&mut self) -> Result<()> {
        let perf_span = perf_span!("data section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        let data_reloc = &self.emit_state.data_relocations;
        let mut section = wasm_encoder::DataSection::new();
        // `(input segment, address, data)`; the address is `None` to copy the input's offset
        let mut segments: Vec<(usize, Option<u64>, Vec<u8>)> = vec![];
        // Every input segment keeps its index, so that indices in the code stay valid.
        for (segment_idx, segment) in data_reloc.per_segment.iter().enumerate() {
            let input_data = &self.input_module.data_segments[segment_idx];
            let (addr_offset, data) = match segment {
                DataSegmentEmitInfo::FromInputInAll => (
                    None,
                    self.get_relocated_segment_data(segment_idx, input_data)?,
                ),
                DataSegmentEmitInfo::FromInputOnlyInMain if self.is_main() => (
                    None,
                    self.get_relocated_segment_data(segment_idx, input_data)?,
                ),
                DataSegmentEmitInfo::FromInputOnlyInMain => {
                    (None, vec![]) // no data, but emit the segment to not shift data indices
                }
                DataSegmentEmitInfo::Ranges { base_address, .. } => {
                    match data_reloc
                        .fragments(segment_idx, self.output_module_index)
                        .next()
                    {
                        Some(fragment) => (
                            Some(base_address + fragment.offset),
                            self.fragment_data(segment_idx, fragment)?,
                        ),
                        None => (None, vec![]),
                    }
                }
            };
            segments.push((segment_idx, addr_offset, data));
        }
        // Further fragments are appended, see `DataEmitInfo::extra_fragments`.
        for (segment_idx, fragment) in data_reloc.extra_fragments(self.output_module_index) {
            let DataSegmentEmitInfo::Ranges { base_address, .. } =
                &data_reloc.per_segment[segment_idx]
            else {
                unreachable!("fragments only exist for relocated segments");
            };
            segments.push((
                segment_idx,
                Some(base_address + fragment.offset),
                self.fragment_data(segment_idx, fragment)?,
            ));
        }
        for (segment_idx, addr_offset, data) in segments {
            let input_data = &self.input_module.data_segments[segment_idx];
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
                            if module_offset <= i32::MAX as u64 {
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
        let perf_span = perf_span!("name section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();

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
            for output_function in &self.defined_functions {
                match output_function {
                    OutputFunction::DefinedFromInput { input_func_id, .. } => {
                        let Some(&output_func_id) = self
                            .dep_to_local_index
                            .get(&DepNode::Function(*input_func_id))
                        else {
                            continue;
                        };
                        if let Some(&name) = self.input_module.names.functions.get(input_func_id) {
                            name_map.append(output_func_id as u32, name);
                        }
                        if let Some(name_map) = self.input_module.names.locals.get(input_func_id) {
                            locals_map.append(output_func_id as u32, &convert_name_map(name_map)?);
                        }
                        if let Some(name_map) = self.input_module.names.labels.get(input_func_id) {
                            labels_map.append(output_func_id as u32, &convert_name_map(name_map)?);
                        }
                    }
                    &OutputFunction::IndirectCallShim {
                        ref input_func_id,
                        shim_index: local_index,
                        ..
                    } => {
                        if let Some(&name) = self.input_module.names.functions.get(input_func_id) {
                            name_map
                                .append(local_index as u32, &format!("{name} indirect_call_shim"));
                        }
                    }
                    &OutputFunction::LocalShim {
                        ref input_func_id,
                        shim_index: local_index,
                        ..
                    } => {
                        if let Some(&name) = self.input_module.names.functions.get(input_func_id) {
                            name_map.append(local_index as u32, &format!("{name} local_shim"));
                        }
                    }
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
        {
            // appended fragments are named after their input segment
            let mut data_names = self.input_module.names.data_segments.clone();
            let input_count = self.input_module.data_segments.len();
            for (i, (segment_idx, _)) in self
                .emit_state
                .data_relocations
                .extra_fragments(self.output_module_index)
                .into_iter()
                .enumerate()
            {
                if let Some(name) = self.input_module.names.data_segments.get(&segment_idx) {
                    data_names.insert(input_count + i, name);
                }
            }
            section.data(&convert_name_hash_map(&data_names));
        }
        // tag
        // fields
        // tags
        self.output_module.section(&section);
        Ok(())
        // Type names
    }

    fn generate_wasm_bindgen_sections(&mut self) {
        if !self.is_main() {
            return;
        }
        for custom in &self.input_module.wasm_bindgen_unstable {
            self.output_module.section(&wasm_encoder::CustomSection {
                name: custom.name().into(),
                data: custom.data().into(),
            });
        }
    }

    fn generate_target_features_section(&mut self) {
        if let Some(custom) = self.input_module.target_features.as_ref() {
            self.output_module.section(&wasm_encoder::CustomSection {
                name: custom.name().into(),
                data: custom.data().into(),
            });
        }
    }

    fn generate_producers_section(&mut self) -> Result<()> {
        let mut producers = ProducersSection::new();
        let mut produced_by = ProducersField::new();
        const PROCESSED_BY_FIELD_NAME: &str = "processed-by";

        if self.is_main() {
            // copy the section from input wasm, but insert ourselves
            if let Some(fields) = self.input_module.producers.as_ref() {
                for input_field in fields.clone().into_iter() {
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

    fn generate_debug_sections(&mut self) -> Result<()> {
        if !self.emit_state.input_options.emit_dwarf {
            return Ok(());
        }
        let perf_span = perf_span!("debug section", module_index = self.output_module_index);
        let _perf_span = perf_span.enter();
        dwarf::emit_debug_info(self)
    }
}

/// Seek the length of a uleb that is encoded at the start of the passed buffer
fn encoded_uleb_len(bytes: &[u8]) -> usize {
    let mut pos = 0;
    loop {
        let last_byte = (bytes[pos] & 0x80) == 0;
        pos += 1;
        if last_byte {
            return pos;
        }
    }
}

#[derive(Debug)]
struct MissingFunctionMapping {
    stub_func: InputFuncId,
    referenced: u32,
}

impl std::fmt::Display for MissingFunctionMapping {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "no-reloc func {} references input func {} with no output mapping",
            self.stub_func, self.referenced,
        )
    }
}

impl std::error::Error for MissingFunctionMapping {}

struct FuncReencoder<'a> {
    dep_to_local_index: &'a HashMap<DepNode, InputFuncId>,
    func_id: InputFuncId,
}

impl wasm_encoder::reencode::Reencode for FuncReencoder<'_> {
    type Error = MissingFunctionMapping;

    fn function_index(
        &mut self,
        func: u32,
    ) -> Result<u32, wasm_encoder::reencode::Error<Self::Error>> {
        self.dep_to_local_index
            .get(&DepNode::Function(func as usize))
            .copied()
            .map(|i| i as u32)
            .ok_or(wasm_encoder::reencode::Error::UserError(
                MissingFunctionMapping {
                    stub_func: self.func_id,
                    referenced: func,
                },
            ))
    }
}

pub fn emit_modules<'info, M>(
    program_info: &'info SplitProgramInfo,
    emit_state: &EmitState,
    emit_fn: impl Fn(usize, &'info SplitModuleIdentifier, Vec<u8>) -> M,
) -> Result<Vec<M>> {
    let modules = program_info.output_modules.iter().enumerate();
    modules
        .map(|(output_module_index, (identifier, module))| {
            let emit_span = perf_span!(
                "emit module",
                module_index = output_module_index,
                data_size = tracing::field::Empty
            );
            let _emit_span = emit_span.enter();
            // The main module is written even when empty.
            if !matches!(identifier, SplitModuleIdentifier::Main) && module.is_empty {
                return Ok(None);
            }
            let mut emit_state =
                ModuleEmitState::new(emit_state, output_module_index, program_info);

            emit_state
                .generate()
                .with_context(|| format!("Error generating {:?}", identifier))?;

            let data = emit_state.output_module.finish();
            emit_span.record("data_size", data.len());
            Ok(Some(emit_fn(output_module_index, identifier, data)))
        })
        .filter_map(|res| res.transpose())
        .collect()
}
