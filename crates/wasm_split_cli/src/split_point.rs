use std::collections::{BTreeSet, HashMap, HashSet};

use crate::dep_graph::{DepGraph, DepNode};
use crate::graph_utils::tarjan_scc::{SccEvent, TarjanSccResult};
use crate::read::{ExportId, ImportId, InputFuncId, InputModule};
use eyre::{anyhow, bail, Result};
use lazy_static::lazy_static;
use regex::Regex;
use tracing::{trace, warn};
use wasmparser::TypeRef;

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

pub fn trace_enabled(verbose: bool) -> bool {
    verbose && tracing::event_enabled!(tracing::Level::TRACE)
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
    wbg_rooting_funs: HashSet<DepNode>,
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
    roots.insert(module.main_memory());
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

fn wbg_rooting_funs(_dep_graph: &DepGraph, module: &InputModule) -> HashSet<DepNode> {
    // [wasm-bindgen hack]
    // wasm_bindgen specific hack: we root all functions calling `__wbindgen_describe_cast`.
    // this is explained best with reference to the implementation in
    // https://github.com/wasm-bindgen/wasm-bindgen/blob/8ea6a42f2491ecb53ca08c44399df6ad59caf871/src/rt/mod.rs#L30
    // a non-inline function describes the incoming and outgoing types.
    // since it is generic (to allow later monomorphization), this function can not be exported.
    // calls to this function are then later rewritten by wasm-bindgen to the inserted import.
    let mut users_must_be_in_main = HashSet::new();
    let mut _wbg_describe_cast = None;
    for (import_id, import) in module.imports.iter().enumerate() {
        if import.module != "__wbindgen_placeholder__" || !matches!(import.ty, TypeRef::Func(_)) {
            continue;
        }
        if import.name == "__wbindgen_describe_cast" {
            let func_id = module.imported_func_map.get(&import_id).cloned().unwrap();
            _wbg_describe_cast = Some(func_id);
            users_must_be_in_main.insert(DepNode::Function(func_id));
        }
    }

    // Second part of the hack is left out, which would be needed for externref: we root all functions calling wasm_bindgen imports.
    // these are all in danger of getting rewritten during the "externref" pass.
    // wasm-bindgen currently replaces instructions, converting i32 to externref when calling an "adapter"
    // these must end up in the main module.

    // Note: callers to `__wbindgen_describe_cast` will also get replaced with imports and are then
    // subject to further processing via the described externref pass.

    // TODO: iterating the whole dep graph seems excessive
    // Unfortunately, due to inlining in release mode, this means that large functions end up
    // being forced into main.
    // if let Some(wbg_describe_cast) = wbg_describe_cast {
    //     for (dep, children) in dep_graph {
    //         if children.contains(&DepNode::Function(wbg_describe_cast)) {
    //             //users_must_be_in_main.insert(*dep);
    //         }
    //     }
    // }
    users_must_be_in_main
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
    Chunk(BTreeSet<String>),
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

    fn also_in(&mut self, other: &SplitModuleIdentifier) {
        let mut needed_by = BTreeSet::new();
        match self {
            SplitModuleIdentifier::Main => return,
            SplitModuleIdentifier::Split(split) => {
                needed_by.insert(split.clone());
            }
            // temporarily swap out
            SplitModuleIdentifier::Chunk(chunk) => std::mem::swap(chunk, &mut needed_by),
        };
        match other {
            SplitModuleIdentifier::Main => {
                *self = SplitModuleIdentifier::Main;
                return;
            }
            SplitModuleIdentifier::Split(split) => {
                needed_by.insert(split.clone());
            }
            SplitModuleIdentifier::Chunk(chunk) => {
                needed_by.extend(chunk.iter().cloned());
            }
        };
        if needed_by.len() == 1 {
            *self = SplitModuleIdentifier::Split(needed_by.pop_first().unwrap());
        } else {
            debug_assert!(needed_by.len() > 1);
            match self {
                SplitModuleIdentifier::Chunk(chunk) => std::mem::swap(chunk, &mut needed_by),
                _ => *self = SplitModuleIdentifier::Chunk(needed_by),
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct SplitProgramInfo {
    pub output_modules: Vec<(SplitModuleIdentifier, OutputModuleInfo)>,
    pub split_points: Vec<SplitPoint>,
    pub split_point_exports: HashSet<InputFuncId>,

    pub shared_deps: HashSet<DepNode>,
    pub symbol_output_module: HashMap<DepNode, usize>,
}

fn color_all<It, T>(
    color_map: &mut HashMap<T, SplitModuleIdentifier>,
    roots: It,
    color: SplitModuleIdentifier,
) where
    It: IntoIterator<Item = T>,
    T: Eq + std::hash::Hash,
{
    for root in roots {
        color_map
            .entry(root)
            .and_modify(|module| module.also_in(&color))
            .or_insert(color.clone());
    }
}

pub fn compute_split_modules(
    module: &InputModule,
    dep_graph: &DepGraph,
    split_points: &[SplitPoint],
) -> Result<SplitProgramInfo> {
    let split_points_by_module = get_split_points_by_module(split_points);

    let dep_graph = ModuleGraph {
        dep_graph,
        wbg_rooting_funs: wbg_rooting_funs(dep_graph, module),
    };

    trace!("split_points={split_points:?}");

    let split_func_map: HashMap<InputFuncId, InputFuncId> = split_points
        .iter()
        .map(|split_point| (split_point.import_func, split_point.export_func))
        .collect();
    let all_imports = split_func_map
        .keys()
        .map(|&import| DepNode::Function(import))
        .collect::<HashSet<_>>();

    let mut scc = TarjanSccResult::new();
    let mut scc_colors = HashMap::new();

    let main_roots = get_main_module_roots(module, split_points);
    let main_scc_roots = scc.explore(
        &main_roots,
        &dep_graph.dep_graph,
        &dep_graph.wbg_rooting_funs,
    );
    color_all(&mut scc_colors, main_scc_roots, SplitModuleIdentifier::Main);

    // Determine reachable symbols (excluding main module symbols) for each
    // split module. Symbols may be reachable from more than one split module;
    // these symbols will be moved to a separate module.
    for (module_name, entry_points) in &split_points_by_module {
        let roots = get_split_roots(entry_points);
        let scc_roots = scc.explore(&roots, &dep_graph.dep_graph, &dep_graph.wbg_rooting_funs);
        color_all(
            &mut scc_colors,
            scc_roots,
            SplitModuleIdentifier::Split(module_name.clone()),
        );
    }

    let scc_topsort = scc.into_topsort();
    let mut module_coloring = SplitModuleIdentifier::Main;
    let mut scc_split_module_contents = HashMap::<SplitModuleIdentifier, OutputModuleInfo>::new();
    for event in scc_topsort.into_iter().rev() {
        match event {
            SccEvent::Next {
                this,
                out_edges,
                marked_for_main,
            } => {
                module_coloring = scc_colors.remove(&this).unwrap();
                if marked_for_main {
                    module_coloring = SplitModuleIdentifier::Main;
                }
                for dep in out_edges {
                    scc_colors
                        .entry(dep)
                        .and_modify(|module| module.also_in(&module_coloring))
                        .or_insert_with(|| module_coloring.clone());
                }
            }
            SccEvent::Member { node } if all_imports.contains(&node) => {
                // drop imports
            }
            SccEvent::Member { node } => {
                scc_split_module_contents
                    .entry(module_coloring.clone())
                    .or_default()
                    .included_symbols
                    .insert(node);
            }
        }
    }

    let mut split_module_contents = scc_split_module_contents;
    let mut program_info = SplitProgramInfo::default();
    for out_module in split_module_contents.values_mut() {
        let needed_symbols = out_module
            .included_symbols
            .iter()
            .filter_map(|dep| dep_graph.dep_graph.get(dep))
            .flatten()
            .cloned()
            // We share two symbols always:
            // - memory 0 since no relocations for memories exist, and this is our main memory
            // - the indirect function table. We could track the need for this via usages of `TableIndex` relocations (and some other uses),
            //   but this almost always will be needed, anyway, so the analysis would be expensive for little optimization gain.
            //   Instead, we decide on imports of the indirect function table per module, but always export it (from the main module).
            .chain([module.main_memory(), module.indirect_function_table()]);
        for mut dep_to_check in needed_symbols {
            if let DepNode::Function(called_func_id) = &mut dep_to_check {
                // dependencies on module-entries are converted to their exposed impl
                if let Some(mapped_func_id) = split_func_map.get(called_func_id) {
                    *called_func_id = *mapped_func_id;
                }
            }
            let in_other_module = !out_module.included_symbols.contains(&dep_to_check);
            if !in_other_module {
                continue;
            }
            // data symbols need no tracking for sharing, as long as they are defined
            // when needed, as they don't need to be imported or shimmed.
            if let DepNode::DataSymbol(_) = dep_to_check {
                continue;
            }
            out_module.used_shared_deps.insert(dep_to_check);
            program_info.shared_deps.insert(dep_to_check);
        }
    }

    for split_point in split_points {
        program_info
            .shared_deps
            .insert(DepNode::Function(split_point.export_func));
        program_info
            .split_point_exports
            .insert(split_point.export_func);
        program_info.split_points.push(split_point.clone());
    }
    for out_module in split_points_by_module.keys() {
        // TODO: this could be optimized to not emit any wasm module, in some cases.
        // An optimization pass can sometimes merge multiple exported functions across split modules, when
        // two modules end up doing "the same thing". In that case, a module might be completely empty.
        // We still instantiate one, mainly because the loader must still exist. Often, in this case, a chunk
        // shared by all these "merged" modules contains most of the implementation which must still be loaded.
        let out_module = split_module_contents
            .entry(SplitModuleIdentifier::Split(out_module.clone()))
            .or_default();
        out_module.used_shared_deps.insert(module.main_memory());
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
