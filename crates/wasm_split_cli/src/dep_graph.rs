use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::Range,
};

use anyhow::{bail, Context};
use wasmparser::RelocationEntry;

use crate::{
    read::{GlobalId, InputFuncId, InputModule, MemoryId, SymbolIndex, TableId, TagId},
    reloc::RelocVisitor,
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

struct DepRelocVisitor(u32);
impl RelocVisitor for DepRelocVisitor {
    type Result = anyhow::Result<Option<DepNode>>;
    fn visit_type_index(self, _: usize, _: &wasmparser::SymbolInfo<'_>) -> Self::Result {
        Ok(None)
    }
    fn visit_memory_addr(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        _: &str,
        _: Option<&wasmparser::DefinedDataSymbol>,
    ) -> Self::Result {
        Ok(Some(DepNode::DataSymbol(self.0 as usize)))
    }
    fn visit_table_index(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        index: InputFuncId,
        _name: Option<&str>,
    ) -> Self::Result {
        // If an instruction takes the "address" of a function, that function needs to be loaded too.
        Ok(Some(DepNode::Function(index)))
    }
    fn visit_rel_table_index(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        index: InputFuncId,
        _name: Option<&str>,
    ) -> Self::Result {
        Ok(Some(DepNode::Function(index)))
    }
    fn visit_function_index(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        index: InputFuncId,
        _name: Option<&str>,
    ) -> Self::Result {
        Ok(Some(DepNode::Function(index)))
    }
    fn visit_table_number(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        index: TableId,
        _name: Option<&str>,
    ) -> Self::Result {
        Ok(Some(DepNode::Table(index)))
    }
    fn visit_global_index(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        index: GlobalId,
        _name: Option<&str>,
    ) -> Self::Result {
        Ok(Some(DepNode::Global(index)))
    }
    fn visit_tag_index(
        self,
        _symbol_index: usize,
        _flags: wasmparser::SymbolFlags,
        index: TagId,
        _name: Option<&str>,
    ) -> Self::Result {
        Ok(Some(DepNode::Tag(index)))
    }
}

fn shift_range(range: Range<usize>, offset: usize) -> Range<usize> {
    (range.start + offset)..(range.end + offset)
}

pub fn get_dependencies(module: &InputModule) -> anyhow::Result<DepGraph> {
    let mut deps = DepGraph::new();
    let mut add_dep = |a: DepNode, relocation: &RelocationEntry| -> Result<(), anyhow::Error> {
        let target = module
            .reloc_info
            .visit_relocation(relocation, DepRelocVisitor(relocation.index))?;
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
) -> anyhow::Result<usize> {
    let func_index = find_by_range(&module.defined_funcs, &range, |defined_func| {
        defined_func.body.range()
    })
    .with_context(|| format!("No match for function relocation range {range:?}"))?;
    Ok(module.imported_funcs.len() + func_index)
}

fn find_data_symbol_containing_range(
    module: &crate::read::InputModule,
    range: Range<usize>,
) -> anyhow::Result<usize> {
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
) -> anyhow::Result<usize> {
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
