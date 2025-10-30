use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use eyre::{anyhow, bail, Result};
use wasmparser::RelocationEntry;

use crate::{
    read::{GlobalId, InputFuncId, InputModule, MemoryId, SymbolIndex, TableId, TagId},
    reloc::{DataSymbol, RelocDetails},
    util::shift_range,
};

#[derive(Debug, PartialEq, Eq, Hash, Copy, PartialOrd, Ord, Clone)]
pub enum DepNode {
    Function(InputFuncId),
    DataSymbol(SymbolIndex),
    Global(GlobalId),
    Table(TableId),
    Tag(TagId),
    Memory(MemoryId),
}

pub type DepGraph = HashMap<DepNode, HashSet<DepNode>>;

pub fn get_dependencies(module: &InputModule) -> Result<DepGraph> {
    struct Builder<'a, 'm>(DepGraph, &'a InputModule<'m>);
    impl Builder<'_, '_> {
        fn add_dep(&mut self, a: DepNode, b: DepNode) {
            self.0.entry(a).or_default().insert(b);
        }
        fn add_reloc_dep(&mut self, a: DepNode, relocation: &RelocationEntry) -> Result<()> {
            let target = match self.1.reloc_info.expand_relocation(relocation)? {
                RelocDetails::TypeIndex { .. } => None,
                RelocDetails::MemoryAddr(details) => {
                    Some(DepNode::DataSymbol(details.symbol_index))
                }
                RelocDetails::TableIndex(details) => Some(DepNode::Function(details.index)),
                RelocDetails::RelTableIndex(details) => Some(DepNode::Function(details.index)),
                RelocDetails::FunctionIndex(details) => Some(DepNode::Function(details.index)),
                RelocDetails::TableNumber(details) => Some(DepNode::Table(details.index)),
                RelocDetails::GlobalIndex(details) => Some(DepNode::Global(details.index)),
                RelocDetails::TagIndex(details) => Some(DepNode::Tag(details.index)),
            };
            if let Some(target) = target {
                self.add_dep(a, target);
            };
            Ok(())
        }
    }

    let mut deps = Builder(DepGraph::new(), module);

    for dep_entry in iter_functions_with_relocs(module) {
        let (func_index, entry) = dep_entry?;
        deps.add_reloc_dep(DepNode::Function(func_index), entry)?;
    }

    for dep_entry in iter_data_dependencies(module) {
        match dep_entry? {
            DataDependency::Reloc(data_symbol, entry) => {
                deps.add_reloc_dep(DepNode::DataSymbol(data_symbol.symbol_index), entry)?
            }
            DataDependency::Containment { container, inner } => {
                deps.add_dep(
                    DepNode::DataSymbol(container.symbol_index),
                    DepNode::DataSymbol(inner.symbol_index),
                );
            }
            DataDependency::DirectedOverlap {
                from,
                to,
                back_to,
                container,
            } => {
                if let Some(container) = container {
                    deps.add_dep(
                        DepNode::DataSymbol(container.symbol_index),
                        DepNode::DataSymbol(to.symbol_index),
                    );
                }
                deps.add_dep(
                    DepNode::DataSymbol(from.symbol_index),
                    DepNode::DataSymbol(to.symbol_index),
                );
                deps.add_dep(
                    DepNode::DataSymbol(to.symbol_index),
                    DepNode::DataSymbol(back_to.symbol_index),
                );
            }
        }
    }
    Ok(deps.0)
}

fn iter_functions_with_relocs<'m>(
    module: &'m InputModule,
) -> impl Iterator<Item = Result<(InputFuncId, &'m RelocationEntry)>> {
    let code_relocs = module.reloc_info.iter_code_relocs();
    let code_section_offset = module.reloc_info.code_section_offset();
    let mut function_index = 0;
    code_relocs.map(move |entry| {
        let reloc_file_range = shift_range(entry.relocation_range(), code_section_offset);
        // We do an exponential search for a function that contains the relocation's target range.
        let found_index = crate::util::exponential_partition_point(
            &module.defined_funcs[function_index..],
            // We need a monotone predicate: whatever the function's body ends before the relocation range.
            // Uses the fact that `reloc_file_range` is never empty and fully contained in the funcs body.
            |func| reloc_file_range.end > func.body.range().end,
        );
        function_index += found_index;
        if function_index >= module.defined_funcs.len() {
            bail!("Invalid relocation entry {entry:?}, no function contains its relocation range")
        }
        let func_index = module.imported_funcs.len() + function_index;
        Ok((func_index, entry))
    })
}

enum DataDependency<'a> {
    // Relocation entry targets a data symbol
    Reloc(&'a DataSymbol, &'a RelocationEntry),
    // A datasymbol contains another data symbol.
    // We insert a dependency from the larger symbol on the contained symbol, because it is "free".
    // Any split requiring the larger symbol can also require the smaller one without any size increase.
    // It is also required, because `Self::Reloc` reports only the smallest datasymbol the relocation
    // is targeting as the dependent.
    Containment {
        container: &'a DataSymbol,
        inner: &'a DataSymbol,
    },
    // Two (or more) datasymbols are overlapping.
    // I'm not super sure this case is very common or even exists. It is the most complicated to handle.
    // We report up to four symbols. 'to' overlaps both 'from' and 'back_to'. 'back_to' also implies a dependency
    // on 'from' through containment or overlap. Finally 'container', if any, contains 'to'.
    // All overlapping items will imply dependencies among themselves. This is important, as otherwise
    // memory might get initialized more than once.
    DirectedOverlap {
        from: &'a DataSymbol,
        to: &'a DataSymbol,
        back_to: &'a DataSymbol,
        container: Option<&'a DataSymbol>,
    },
}

macro_rules! emit_iter_err {
    ($a:expr) => {
        match $a {
            Err(e) => return Some(Err(e.into())),
            Ok(o) => o,
        }
    };
}

fn iter_data_dependencies<'m>(
    module: &'m InputModule,
) -> impl Iterator<Item = Result<DataDependency<'m>>> {
    let data_section_offset = module.reloc_info.data_section_offset();
    let mut data_relocs = module.reloc_info.iter_data_relocs().peekable();
    let mut data_symbols = module.reloc_info.data_symbols.iter().peekable();

    // You should read the following as a generator, that yields dependencies one-by-one. "Real" generators
    // are still unstable though.

    // We go through both data relocs and symbols in one pass, picking up dependencies as we go.
    // Keep in mind that `data_symbols` and `data_relocs` are both sorted by start offset.
    // Keep a stack of datasymbols, reverse sorted by end, of symbols that might still overlap.
    let mut overlap_candidates: Vec<&DataSymbol> = vec![];
    std::iter::from_fn(move || loop {
        if let Some(&entry) = data_relocs.peek() {
            let reloc_file_range = shift_range(entry.relocation_range(), data_section_offset);
            let should_handle_reloc = match data_symbols.peek() {
                None => true,
                Some(next_symbol) => next_symbol.range.start >= reloc_file_range.end,
            };
            if should_handle_reloc {
                let entry = data_relocs.next().unwrap();
                // pop until we overlap
                while overlap_candidates.last().is_some_and(|&back| {
                    // !reloc_file_range.is_overlapping(back.range)
                    !((reloc_file_range.start < back.range.end)
                        & (back.range.start < reloc_file_range.end))
                }) {
                    let _ = overlap_candidates.pop();
                }
                let &target = emit_iter_err!(overlap_candidates.last().ok_or_else(|| anyhow!(
                    "Invalid relocation entry {entry:?} not overlapping any data symbols"
                )));
                if !(target.range.start <= reloc_file_range.start
                    && reloc_file_range.end <= target.range.end)
                {
                    emit_iter_err!(Err(anyhow!("Invalid relocation entry {entry:?} not fully contained inside its data symbol")))
                }
                return Some(Ok(DataDependency::Reloc(target, entry)));
            }
        }

        let next_symbol = data_symbols.next()?;
        // First pop off all symbols that come before the next one (no overlaps)
        while overlap_candidates
            .last()
            .is_some_and(|&candidate| candidate.range.end <= next_symbol.range.start)
        {
            overlap_candidates.pop();
        }

        // next_symbol now overlaps or is contained in all items on the stack
        let mut container = None;
        let last = overlap_candidates.last().cloned(); // saved from mutability below
        'candidate_loop: for candidate in &mut overlap_candidates {
            let is_contained = next_symbol.range.end <= candidate.range.end;
            debug_assert!(
                !is_contained || candidate.range.start <= next_symbol.range.start,
                "input not sorted"
            );
            if is_contained {
                container = Some(*candidate);
                continue 'candidate_loop;
            }
            // we found an overlap! Replace this candidate with next_symbol, then
            let back_to = std::mem::replace(candidate, next_symbol);
            return Some(Ok(DataDependency::DirectedOverlap {
                from: last.unwrap(),
                to: next_symbol,
                back_to,
                container,
            }));
        }

        // if we reach here, then the next symbol is contained in all items on the stack
        if let Some(container) = container {
            return Some(Ok(DataDependency::Containment {
                container,
                inner: next_symbol,
            }));
        };

        // No item on the stack, make this the only one and check the next one
        overlap_candidates.push(next_symbol);
    })
}
