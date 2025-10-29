use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::Range,
};

use eyre::{bail, Context, Result};
use wasmparser::RelocationEntry;

use crate::{
    read::{GlobalId, InputFuncId, InputModule, MemoryId, SymbolIndex, TableId, TagId},
    reloc::{DataSymbol, RelocDetails},
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

fn shift_range(range: Range<usize>, offset: usize) -> Range<usize> {
    (range.start + offset)..(range.end + offset)
}

pub fn get_dependencies(module: &InputModule) -> Result<DepGraph> {
    let mut deps = DepGraph::new();
    let mut add_dep = |a: DepNode, relocation: &RelocationEntry| -> Result<()> {
        let target = match module.reloc_info.expand_relocation(relocation)? {
            RelocDetails::TypeIndex { .. } => None,
            RelocDetails::MemoryAddr(details) => Some(DepNode::DataSymbol(details.symbol_index)),
            RelocDetails::TableIndex(details) => Some(DepNode::Function(details.index)),
            RelocDetails::RelTableIndex(details) => Some(DepNode::Function(details.index)),
            RelocDetails::FunctionIndex(details) => Some(DepNode::Function(details.index)),
            RelocDetails::TableNumber(details) => Some(DepNode::Table(details.index)),
            RelocDetails::GlobalIndex(details) => Some(DepNode::Global(details.index)),
            RelocDetails::TagIndex(details) => Some(DepNode::Tag(details.index)),
        };
        if let Some(target) = target {
            deps.entry(a).or_default().insert(target);
        };
        Ok(())
    };

    for dep_entry in module.iter_functions_with_relocs() {
        let (func_index, entry) = dep_entry?;
        add_dep(DepNode::Function(func_index), entry)?;
    }

    for dep_entry in module.iter_data_symbols_with_relocs() {
        let (data_symbol, entry) = dep_entry?;
        add_dep(DepNode::DataSymbol(data_symbol.symbol_index), entry)?;
    }
    Ok(deps)
}

fn find_by_range<T: Debug, U: Debug + Ord, F: Fn(&T) -> Range<U>>(
    items: &[T],
    range: &Range<U>,
    get_range: F,
) -> Result<usize> {
    let matching_range = super::util::find_by_range(items, range, &get_range);
    let index = matching_range.start;
    if matching_range.len() != 1 {
        bail!(
            "Prev range is: {:?}, next range is: {:?}",
            items.get(index - 1).map(|item| (item, get_range(item))),
            items.get(index).map(|item| (item, get_range(item)))
        )
    }
    Ok(index)
}

impl<'a> InputModule<'a> {
    fn iter_functions_with_relocs(
        &self,
    ) -> impl Iterator<Item = Result<(InputFuncId, &'_ RelocationEntry)>> {
        let code_relocs = self.reloc_info.iter_code_relocs();
        let mut function_index = 0;
        code_relocs.map(move |entry| {
            let reloc_file_range = shift_range(
                entry.relocation_range(),
                self.reloc_info.code_section_offset(),
            );
            // We do an exponential search for a function that containsthe relocation's target range.
            let found_index = crate::util::exponential_partition_point(
                &self.defined_funcs[function_index..],
                // We need a monotone predicate: whatever the function's body ends before the relocation range.
                // Uses the fact that `reloc_file_range` is never empty and fully contained in the funcs body.
                |func| reloc_file_range.end > func.body.range().end,
            );
            function_index += found_index;
            if function_index >= self.defined_funcs.len() {
                bail!(
                    "Invalid relocation entry {entry:?}, no function contains its relocation range"
                )
            }
            let func_index = self.imported_funcs.len() + function_index;
            Ok((func_index, entry))
        })
    }
    fn iter_data_symbols_with_relocs(
        &self,
    ) -> impl Iterator<Item = Result<(&'_ DataSymbol, &'_ RelocationEntry)>> {
        let data_relocs = self.reloc_info.iter_data_relocs();
        data_relocs.map(move |entry| {
            let reloc_file_range = shift_range(
                entry.relocation_range(),
                self.reloc_info.data_section_offset(),
            );
            let index = find_by_range(
                &self.reloc_info.data_symbols,
                &reloc_file_range,
                |data_symbol| data_symbol.range.clone(),
            )
            .with_context(|| format!("No match for data relocation range {reloc_file_range:?}"))
            .with_context(|| format!("Invalid relocation entry {entry:?}"))?;
            Ok((&self.reloc_info.data_symbols[index], entry))
        })
    }
}
