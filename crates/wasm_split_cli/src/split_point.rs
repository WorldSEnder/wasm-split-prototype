use std::collections::{HashMap, HashSet, VecDeque};

use crate::dep_graph::{DepGraph, DepNode};
use crate::read::{ExportId, ImportId, InputFuncId, InputModule};
use anyhow::{anyhow, bail};
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SplitPoint {
    pub module_name: String,
    pub import: ImportId,
    pub import_func: InputFuncId,
    pub export: ExportId,
    pub export_func: InputFuncId,
}

pub fn get_split_points(module: &InputModule) -> anyhow::Result<Vec<SplitPoint>> {
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
        .map(|(key, import_id)| -> anyhow::Result<SplitPoint> {
            let export_id = export_map.remove(&key).ok_or_else(|| {
                anyhow::anyhow!("No corresponding export for split import {key:?}")
            })?;
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
        .collect::<anyhow::Result<Vec<SplitPoint>>>()?;

    #[allow(clippy::never_loop)]
    for (key, _) in export_map.iter() {
        anyhow::bail!("No corresponding import for split export {key:?}");
    }

    Ok(split_points)
}

#[derive(Debug, Default)]
pub struct ReachabilityGraph {
    pub reachable: HashSet<DepNode>,
    pub parents: HashMap<DepNode, DepNode>,
}

#[derive(Debug, Default)]
pub struct OutputModuleInfo {
    pub included_symbols: HashSet<DepNode>,
    pub parents: HashMap<DepNode, DepNode>, // for debug only
    pub split_points: Vec<SplitPoint>,
    pub used_shared_deps: HashSet<DepNode>,
}

impl OutputModuleInfo {
    pub fn print(&self, module_name: &str, module: &InputModule) {
        print_deps(module_name, module, &self.included_symbols, &self.parents);
    }
}

impl From<ReachabilityGraph> for OutputModuleInfo {
    fn from(reachability: ReachabilityGraph) -> Self {
        Self {
            included_symbols: reachability.reachable,
            parents: reachability.parents,
            ..Default::default()
        }
    }
}

fn print_deps(
    module_name: &str,
    module: &InputModule,
    reachable: &HashSet<DepNode>,
    parents: &HashMap<DepNode, DepNode>,
) {
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

    println!("SPLIT: ============== {module_name}");
    let mut total_size: usize = 0;
    for dep in reachable.iter() {
        if let DepNode::Function(index) = dep {
            let size = index
                .checked_sub(module.imported_funcs.len())
                .map(|defined_index| module.defined_funcs[defined_index].body.range().len())
                .unwrap_or_default();
            total_size += size;
            println!("   {} size={size:?}", format_dep(dep));
        } else {
            println!("   {}", format_dep(dep));
        }
        let mut node = dep;
        while let Some(parent) = parents.get(node) {
            println!("      <== {}", format_dep(parent));
            node = parent;
        }
    }
    println!("SPLIT: ============== {module_name}  : total size: {total_size}");
}

pub fn find_reachable_deps(
    deps: &DepGraph,
    roots: &HashSet<DepNode>,
    exclude: &HashSet<DepNode>,
) -> ReachabilityGraph {
    let mut queue: VecDeque<DepNode> = roots.iter().copied().collect();
    let mut seen = HashSet::<DepNode>::new();
    let mut parents = HashMap::<DepNode, DepNode>::new();
    while let Some(node) = queue.pop_front() {
        seen.insert(node);
        let Some(children) = deps.get(&node) else {
            continue;
        };
        for child in children {
            if seen.contains(child) || exclude.contains(child) {
                continue;
            }
            parents.entry(*child).or_insert(node);
            queue.push_back(*child);
        }
    }
    ReachabilityGraph {
        reachable: seen,
        parents,
    }
}

pub fn get_main_module_roots(
    module: &InputModule,
    split_points: &[SplitPoint],
) -> HashSet<DepNode> {
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
    for split_point in split_points.iter() {
        roots.remove(&DepNode::Function(split_point.export_func));
        roots.remove(&DepNode::Function(split_point.import_func));
    }
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
            Self::Main => "main".to_string(),
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
) -> anyhow::Result<SplitProgramInfo> {
    let split_points_by_module = get_split_points_by_module(split_points);

    println!("split_points={split_points:?}");

    let split_func_map: HashMap<InputFuncId, InputFuncId> = split_points
        .iter()
        .map(|split_point| (split_point.import_func, split_point.export_func))
        .collect();

    let find_reachable_non_ignored_deps = |roots: &HashSet<DepNode>, exclude: &HashSet<DepNode>| {
        let mut deps = find_reachable_deps(dep_graph, roots, exclude);
        for split_point in split_points.iter() {
            deps.reachable
                .remove(&DepNode::Function(split_point.import_func));
        }
        deps
    };

    let main_roots = get_main_module_roots(module, split_points);
    let main_deps = find_reachable_non_ignored_deps(&main_roots, &HashSet::new());

    // Determine reachable symbols (excluding main module symbols) for each
    // split module. Symbols may be reachable from more than one split module;
    // these symbols will be moved to a separate module.
    let mut split_module_candidates: HashMap<String, ReachabilityGraph> = split_points_by_module
        .iter()
        .map(|(module_name, entry_points)| {
            let mut roots = HashSet::<DepNode>::new();
            for entry_point in entry_points.iter() {
                roots.insert(DepNode::Function(entry_point.export_func));
            }
            let split_deps = find_reachable_non_ignored_deps(&roots, &main_deps.reachable);
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
            let Some(deps) = dep_graph.get(symbol) else {
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
