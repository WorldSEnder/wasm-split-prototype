use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::Range,
};

use eyre::{bail, Context, Result};
use wasmparser::RelocationEntry;

use crate::{
    read::{GlobalId, InputFuncId, InputModule, MemoryId, SymbolIndex, TableId, TagId},
    reloc::RelocDetails,
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

    for entry in module.reloc_info.iter_code_relocs() {
        let func_index = find_function_containing_range(
            module,
            shift_range(
                entry.relocation_range(),
                module.reloc_info.code_section_offset(),
            ),
        )
        .with_context(|| format!("Invalid relocation entry {entry:?}"))?;
        add_dep(DepNode::Function(func_index), entry)?;
    }

    for entry in module.reloc_info.iter_data_relocs() {
        let symbol_index = find_data_symbol_containing_range(
            module,
            shift_range(
                entry.relocation_range(),
                module.reloc_info.data_section_offset(),
            ),
        )
        .with_context(|| format!("Invalid relocation entry {entry:?}"))?;
        add_dep(DepNode::DataSymbol(symbol_index), entry)?;
    }
    Ok(deps)
}

fn find_function_containing_range(
    module: &crate::read::InputModule,
    range: Range<usize>,
) -> Result<usize> {
    let func_index = find_by_range(&module.defined_funcs, &range, |defined_func| {
        defined_func.body.range()
    })
    .with_context(|| format!("No match for function relocation range {range:?}"))?;
    Ok(module.imported_funcs.len() + func_index)
}

fn find_data_symbol_containing_range(
    module: &crate::read::InputModule,
    range: Range<usize>,
) -> Result<usize> {
    let index = find_by_range(&module.reloc_info.data_symbols, &range, |data_symbol| {
        data_symbol.range.clone()
    })
    .with_context(|| format!("No match for data relocation range {range:?}"))?;
    Ok(module.reloc_info.data_symbols[index].symbol_index)
}

fn find_by_range<T: Debug, U: Debug + Ord, F: Fn(&T) -> Range<U>>(
    items: &[T],
    range: &Range<U>,
    get_range: F,
) -> Result<usize> {
    let matching_range = super::util::find_by_range(items, range, &get_range);
    let index = matching_range.start;
    if matching_range.is_empty() {
        bail!(
            "Prev range is: {:?}, next range is: {:?}",
            items.get(index - 1).map(|item| (item, get_range(item))),
            items.get(index).map(|item| (item, get_range(item)))
        )
    }
    Ok(index)
}
