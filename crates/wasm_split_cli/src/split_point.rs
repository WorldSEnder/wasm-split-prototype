use std::collections::{HashMap, HashSet, VecDeque};

use crate::dep_graph::{DepGraph, DepNode};
use crate::read::{ExportId, ImportId, InputFuncId, InputModule};
use eyre::{anyhow, bail, Result};
use lazy_static::lazy_static;
use regex::Regex;
use tracing::{trace, warn};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SplitPoint {
    pub module_name: String,
    pub import: ImportId,
    pub import_func: InputFuncId,
    pub export: ExportId,
    pub export_func: InputFuncId,
}

pub fn get_split_points(module: &InputModule) -> Result<Vec<SplitPoint>> {
    macro_rules! process_imports_or_exports {
        ($pattern:expr, $map:ident, $member:ident, $id_ty:ty) => {
            let mut $map = HashMap::<(String, String), $id_ty>::new();
            {
                lazy_static! {
                    static ref PATTERN: Regex = Regex::new($pattern).unwrap();
                }

                for (id, item) in module.$member.iter().enumerate() {
                    let Some(captures) = PATTERN.captures(&item.name) else {
                        continue;
                    };
                    let (_, [module_name, unique_id]) = captures.extract();
                    $map.insert((module_name.into(), unique_id.into()), id);
                }
            }
        };
    }

    process_imports_or_exports!(
        "__wasm_split_00(.*)00_import_([0-9a-f]{32})",
        import_map,
        imports,
        ImportId
    );
    process_imports_or_exports!(
        "__wasm_split_00(.*)00_export_([0-9a-f]{32})",
        export_map,
        exports,
        ExportId
    );

    let split_points = import_map
        .drain()
        .map(|(key, import_id)| -> Result<SplitPoint> {
            let export_id = export_map
                .remove(&key)
                .ok_or_else(|| anyhow!("No corresponding export for split import {key:?}"))?;
            let export = module.exports[export_id];
            let wasmparser::Export {
                kind: wasmparser::ExternalKind::Func,
                index,
                ..
            } = export
            else {
                bail!("Expected exported function but received: {export:?}");
            };
            let &import_func = module.imported_func_map.get(&import_id).ok_or_else(|| {
                anyhow!(
                    "Expected imported function but received: {:?}",
                    &module.imports[import_id]
                )
            })?;
            Ok(SplitPoint {
                module_name: key.0,
                import: import_id,
                import_func,
                export: export_id,
                export_func: index as InputFuncId,
            })
        })
        .collect::<Result<Vec<SplitPoint>>>()?;

    if !export_map.is_empty() {
        warn!(
            "No corresponding imports for split export(s) {:?}",
            export_map.keys().collect::<Vec<_>>()
        );
    }

    Ok(split_points)
}

#[derive(Debug, Default)]
pub struct ReachabilityGraph {
    pub reachable: HashSet<DepNode>,
}

#[derive(Debug, Default)]
pub struct OutputModuleInfo {
    pub included_symbols: HashSet<DepNode>,
    pub split_points: Vec<SplitPoint>,
    pub used_shared_deps: HashSet<DepNode>,
}

impl OutputModuleInfo {
    pub fn print(&self, module_name: &str, module: &InputModule) {
        print_deps(module_name, module, &self.included_symbols);
    }
}

impl From<ReachabilityGraph> for OutputModuleInfo {
    fn from(reachability: ReachabilityGraph) -> Self {
        Self {
            included_symbols: reachability.reachable,
            ..Default::default()
        }
    }
}

fn print_deps(module_name: &str, module: &InputModule, reachable: &HashSet<DepNode>) {
    let format_dep = |dep: &DepNode| match dep {
        DepNode::Function(index) => {
            let name = module.names.functions.get(index);
            format!("func[{index}] <{name:?}>")
        }
        DepNode::DataSymbol(index) => {
            let symbol = module.reloc_info.symbols[*index];
            format!("{symbol:?}")
        }
        DepNode::Global(index) => {
            format!("global[{index}]")
        }
        DepNode::Table(index) => {
            format!("table[{index}]")
        }
        DepNode::Tag(index) => {
            format!("tag[{index}]")
        }
        DepNode::Memory(index) => {
            format!("memory[{index}]")
        }
    };

    trace!("SPLIT: ============== {module_name}");
    let mut total_size: usize = 0;
    for dep in reachable.iter() {
        if let DepNode::Function(index) = dep {
            let size = index
                .checked_sub(module.imported_funcs.len())
                .map(|defined_index| module.defined_funcs[defined_index].body.range().len())
                .unwrap_or_default();
            total_size += size;
            trace!("   {} size={size:?}", format_dep(dep));
        } else {
            trace!("   {}", format_dep(dep));
        }
    }
    trace!("SPLIT: ============== {module_name}  : total size: {total_size}");
}

struct ModuleGraph<'a> {
    dep_graph: &'a DepGraph,
    wbg_describe_cast: Option<InputFuncId>,
}

fn find_reachable_deps(
    deps: &ModuleGraph,
    roots: &HashSet<DepNode>,
    main_deps: &HashSet<DepNode>,
    // there are some DepNodes which we want to ensure to put into the main module
    additional_for_main: &mut HashSet<DepNode>,
) -> ReachabilityGraph {
    let mut queue: VecDeque<DepNode> = roots.iter().copied().collect();
    let mut seen = HashSet::<DepNode>::new();
    while let Some(node) = queue.pop_front() {
        seen.insert(node);
        let Some(children) = deps.dep_graph.get(&node) else {
            continue;
        };
        if let Some(wbg_describe_cast) = deps.wbg_describe_cast {
            if children.contains(&DepNode::Function(wbg_describe_cast)) {
                if !main_deps.contains(&node) {
                    additional_for_main.insert(node);
                    continue;
                }
            }
        }
        for child in children {
            if seen.contains(child) || main_deps.contains(child) {
                continue;
            }
            queue.push_back(*child);
        }
    }
    ReachabilityGraph { reachable: seen }
}

fn get_main_module_roots(module: &InputModule, split_points: &[SplitPoint]) -> HashSet<DepNode> {
    let mut roots: HashSet<DepNode> = HashSet::new();
    if let Some(id) = module.start {
        roots.insert(DepNode::Function(id));
    }

    // We root all imports and exports in the main module
    for func_id in 0..module.imported_funcs.len() {
        roots.insert(DepNode::Function(func_id));
    }
    for global_id in 0..module.imported_globals_num {
        roots.insert(DepNode::Global(global_id));
    }
    for table_id in 0..module.imported_tables_num {
        roots.insert(DepNode::Table(table_id));
    }
    for tag_id in 0..module.imported_tags_num {
        roots.insert(DepNode::Tag(tag_id));
    }
    for tag_id in 0..module.imported_memories_num {
        roots.insert(DepNode::Memory(tag_id));
    }
    for wasmparser::Export { index, kind, .. } in module.exports.iter() {
        roots.insert(match kind {
            wasmparser::ExternalKind::Func => DepNode::Function(*index as usize),
            wasmparser::ExternalKind::Table => DepNode::Table(*index as usize),
            wasmparser::ExternalKind::Global => DepNode::Global(*index as usize),
            wasmparser::ExternalKind::Tag => DepNode::Tag(*index as usize),
            wasmparser::ExternalKind::Memory => DepNode::Memory(*index as usize),
        });
    }

    // We root every unused indirect at the root
    for &func_id in &module.reloc_info.visible_indirects {
        roots.insert(DepNode::Function(func_id));
    }

    // finally, remove all splits points they belong in their own module
    for split_point in split_points.iter() {
        roots.remove(&DepNode::Function(split_point.export_func));
        roots.remove(&DepNode::Function(split_point.import_func));
    }
    roots
}

fn get_wbg_describe_cast(module: &InputModule) -> Option<InputFuncId> {
    // wasm_bindgen specific hack: we root all functions calling `__wbindgen_describe_cast`.
    // this is explained best with reference to the implementation in
    // https://github.com/wasm-bindgen/wasm-bindgen/blob/8ea6a42f2491ecb53ca08c44399df6ad59caf871/src/rt/mod.rs#L30
    // a non-inline function describes the incoming and outgoing types.
    // since it is generic (to allow later monomorphization), this function can not be exported.
    // calls to this function are then later rewritten by wasm-bindgen to the inserted import.
    let (wbg_describe_cast_import, _) = module.imports.iter().enumerate().find(|(_, import)| {
        import.module == "__wbindgen_placeholder__" && import.name == "__wbindgen_describe_cast"
    })?;
    module
        .imported_func_map
        .get(&wbg_describe_cast_import)
        .cloned()
}

fn get_split_roots(splits_in_module: &[&SplitPoint]) -> HashSet<DepNode> {
    let mut roots = HashSet::<DepNode>::new();
    for entry_point in splits_in_module {
        roots.insert(DepNode::Function(entry_point.export_func));
    }
    // TODO: handle memories by rooting memory 0 since there are no relocations to help
    //  do this during dependency analysis.
    roots
}

pub fn get_split_points_by_module(
    split_points: &[SplitPoint],
) -> HashMap<String, Vec<&SplitPoint>> {
    split_points
        .iter()
        .fold(HashMap::new(), |mut map, split_point| {
            map.entry(split_point.module_name.clone())
                .or_default()
                .push(split_point);
            map
        })
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub enum SplitModuleIdentifier {
    Main,
    Split(String),
    Chunk(Vec<String>),
}

impl SplitModuleIdentifier {
    pub fn filename(&self, module_index: usize) -> String {
        match self {
            Self::Main => unreachable!("main wasm filepath is handled separately"),
            Self::Split(name) => format!("split_{name}"),
            Self::Chunk(_) => format!("chunk_{module_index}"),
        }
    }
    pub fn loader_name(&self) -> String {
        match self {
            // MUST match the name in wasm_split_macros
            Self::Split(name) => format!("__wasm_split_load_{name}"),
            _ => unreachable!("only whole modules have a loader"),
        }
    }
}

#[derive(Debug, Default)]
pub struct SplitProgramInfo {
    pub output_modules: Vec<(SplitModuleIdentifier, OutputModuleInfo)>,
    pub split_point_exports: HashSet<InputFuncId>,

    pub shared_deps: HashSet<DepNode>,
    pub symbol_output_module: HashMap<DepNode, usize>,
}

pub fn compute_split_modules(
    module: &InputModule,
    dep_graph: &DepGraph,
    split_points: &[SplitPoint],
) -> Result<SplitProgramInfo> {
    let wbg_describe_cast = get_wbg_describe_cast(module);
    let split_points_by_module = get_split_points_by_module(split_points);

    let dep_graph = ModuleGraph {
        dep_graph,
        wbg_describe_cast,
    };

    trace!("split_points={split_points:?}");

    let split_func_map: HashMap<InputFuncId, InputFuncId> = split_points
        .iter()
        .map(|split_point| (split_point.import_func, split_point.export_func))
        .collect();

    let find_reachable_non_ignored_deps =
        |roots: &HashSet<DepNode>, main_deps: &mut ReachabilityGraph| {
            let mut additional_mains = HashSet::new();
            let mut deps = find_reachable_deps(
                &dep_graph,
                roots,
                &main_deps.reachable,
                &mut additional_mains,
            );
            while !additional_mains.is_empty() {
                let added_just_now = std::mem::take(&mut additional_mains);
                let reachable = find_reachable_deps(
                    &dep_graph,
                    &added_just_now,
                    &main_deps.reachable,
                    &mut additional_mains,
                );
                additional_mains.extend(reachable.reachable.difference(&added_just_now));
                main_deps.reachable.extend(reachable.reachable);
            }
            for split_point in split_points.iter() {
                deps.reachable
                    .remove(&DepNode::Function(split_point.import_func));
            }
            deps
        };

    let main_roots = get_main_module_roots(module, split_points);
    let mut additional_mains = ReachabilityGraph::default();
    let mut main_deps = find_reachable_non_ignored_deps(&main_roots, &mut additional_mains);
    main_deps.reachable.extend(additional_mains.reachable);

    // Determine reachable symbols (excluding main module symbols) for each
    // split module. Symbols may be reachable from more than one split module;
    // these symbols will be moved to a separate module.
    let mut split_module_candidates: HashMap<String, ReachabilityGraph> = split_points_by_module
        .iter()
        .map(|(module_name, entry_points)| {
            let roots = get_split_roots(&entry_points);
            let split_deps = find_reachable_non_ignored_deps(&roots, &mut main_deps);
            (module_name.clone(), split_deps)
        })
        .collect();

    // Set of split modules from which each symbol is reachable.
    let mut dep_candidate_modules = HashMap::<DepNode, Vec<String>>::new();
    for (module_name, deps) in split_module_candidates.iter() {
        for dep in deps.reachable.iter() {
            dep_candidate_modules
                .entry(*dep)
                .or_default()
                .push(module_name.clone());
        }
    }

    let mut split_module_contents = HashMap::<SplitModuleIdentifier, OutputModuleInfo>::new();
    split_module_contents.insert(SplitModuleIdentifier::Main, main_deps.into());
    for (dep, mut modules) in dep_candidate_modules {
        if modules.len() > 1 {
            modules.sort();
            for module in modules.iter() {
                let module_contents = split_module_candidates.get_mut(module).unwrap();
                module_contents.reachable.remove(&dep);
            }
            split_module_contents
                .entry(SplitModuleIdentifier::Chunk(modules))
                .or_default()
                .included_symbols
                .insert(dep);
        }
    }
    split_module_contents.extend(
        split_module_candidates
            .drain()
            .map(|(module_name, deps)| (SplitModuleIdentifier::Split(module_name), deps.into())),
    );

    let mut program_info = SplitProgramInfo::default();
    for module in split_module_contents.values_mut() {
        for symbol in module.included_symbols.iter() {
            let Some(deps) = dep_graph.dep_graph.get(symbol) else {
                continue;
            };
            for &(mut dep_to_check) in deps {
                if let DepNode::Function(called_func_id) = &mut dep_to_check {
                    // dependencies on module-entries are converted to their exposed impl
                    if let Some(mapped_func_id) = split_func_map.get(called_func_id) {
                        *called_func_id = *mapped_func_id;
                    }
                }
                let in_other_module = !module.included_symbols.contains(&dep_to_check);
                if !in_other_module {
                    continue;
                }
                // data symbols need no tracking for sharing, as long as they are defined
                // when needed, as they don't need to be imported or shimmed.
                if let DepNode::DataSymbol(_) = dep_to_check {
                    continue;
                }
                module.used_shared_deps.insert(dep_to_check);
                program_info.shared_deps.insert(dep_to_check);
            }
        }
    }

    for split_point in split_points {
        program_info
            .shared_deps
            .insert(DepNode::Function(split_point.export_func));
        let output_module = split_module_contents
            .get_mut(&SplitModuleIdentifier::Split(
                split_point.module_name.to_string(),
            ))
            .unwrap();
        program_info
            .split_point_exports
            .insert(split_point.export_func);
        output_module.split_points.push(split_point.clone());
    }

    program_info.output_modules = split_module_contents.drain().collect();
    program_info
        .output_modules
        .sort_by_key(|(identifier, _)| (*identifier).clone());

    for (output_index, (_, info)) in program_info.output_modules.iter().enumerate() {
        for &symbol in info.included_symbols.iter() {
            program_info
                .symbol_output_module
                .insert(symbol, output_index);
        }
    }

    Ok(program_info)
}
