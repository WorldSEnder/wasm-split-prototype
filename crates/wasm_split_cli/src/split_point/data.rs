use std::{collections::HashMap, ops::Range};

use eyre::Result;
use tracing::{trace, warn};
use wasmparser::{DataKind, DefinedDataSymbol, SegmentFlags, SymbolInfo};

use crate::{
    dep_graph::DepNode,
    read::InputModule,
    split_point::{SplitModuleIdentifier, MAIN_MODULE},
    util::wasm_data_len,
};

use super::{OutputModuleInfo, SplitProgramInfo};

#[derive(Debug)]
pub struct LateDataRange {
    pub input_range: Range<u64>,
    // the modules whose symbols are in this range, see `SplitModuleIdentifier::also_in`
    needed_by: SplitModuleIdentifier,
    // the module emitting this range, which is loaded whenever any of `needed_by` is
    in_module: usize,
    /// Power of 2. The alignment inferred for the range's first symbol, raised only by
    /// partially overlapping symbols; a contained symbol cannot be aligned stricter than
    /// its container. The range is placed at a multiple of it.
    data_align: u64,
    // offset in the relocated segment, filled in by `layout_ranges`
    pub segment_offset: u64,
}

/// A contiguous run of ranges of one output module, emitted as one data segment.
#[derive(Debug)]
pub struct Fragment {
    // offset in the relocated segment,
    // also accessible as the `LateDataRange::segment_offset` of the first referenced range
    pub offset: u64,
    // the ranges, as a range of indices into `RangeLayout::emit_order`
    pub ranges: Range<usize>,
}

#[derive(Debug)]
pub struct RangeLayout {
    // indices into the ranges, in the order they are placed in the segment
    pub emit_order: Vec<usize>,
    // in placement order; the fragments of one module are in ascending order
    fragments: Vec<(usize, Fragment)>,
    segment_len: u64,
}

/// Lay out the ranges of one segment.
///
/// Ranges are placed by decreasing alignment, then module and input order. Every range
/// starts at a multiple of its alignment, and linker output gives lengths that are multiples
/// of it, so no padding arises. Each module emits one segment per run of its ranges.
/// The overlong check remains as a safety net.
fn layout_ranges(ranges: &mut [LateDataRange]) -> RangeLayout {
    let mut emit_order: Vec<_> = (0..ranges.len()).collect();
    emit_order.sort_by_key(|&range_idx| {
        let range = &ranges[range_idx];
        (
            std::cmp::Reverse(range.data_align),
            range.in_module,
            range.input_range.start,
        )
    });
    let mut fragments: Vec<(usize, Fragment)> = vec![];
    let mut segment_len: u64 = 0;
    for (order_idx, &range_idx) in emit_order.iter().enumerate() {
        let range = &mut ranges[range_idx];
        range.segment_offset = segment_len.next_multiple_of(range.data_align);
        segment_len = range.segment_offset + (range.input_range.end - range.input_range.start);
        match fragments.last_mut() {
            Some(&mut (module, ref mut fragment)) if module == range.in_module => {
                fragment.ranges.end = order_idx + 1;
            }
            _ => fragments.push((
                range.in_module,
                Fragment {
                    offset: range.segment_offset,
                    ranges: order_idx..order_idx + 1,
                },
            )),
        }
    }
    RangeLayout {
        emit_order,
        fragments,
        segment_len,
    }
}

#[derive(Debug)]
pub enum DataSegmentEmitInfo {
    // Copy this segment from the input, either in all or a specific output module
    FromInputInAll,
    FromInputOnlyIn(usize),
    Ranges {
        // some reloc information
        base_address: u64,
        // the output segments are formed by concatenating runs of these ranges
        ranges: Vec<LateDataRange>,
        // symbol index -> (index in 'ranges', offset in range)
        range_lookup: HashMap<usize, (usize, u64)>,
        layout: RangeLayout,
    },
}

#[derive(Default, Debug)]
pub struct DataEmitInfo {
    pub per_segment: Vec<DataSegmentEmitInfo>,
}

impl DataEmitInfo {
    /// The fragments of `segment_idx` that `module` emits, in ascending order.
    pub fn fragments(&self, segment_idx: usize, module: usize) -> impl Iterator<Item = &Fragment> {
        let fragments = match &self.per_segment[segment_idx] {
            DataSegmentEmitInfo::Ranges { layout, .. } => layout.fragments.as_slice(),
            _ => &[],
        };
        fragments
            .iter()
            .filter_map(move |(frag_module, fragment)| (*frag_module == module).then_some(fragment))
    }

    /// Every input segment keeps its index in every output module, holding the module's first
    /// fragment of it (or nothing). Further fragments are appended after all input segments;
    /// this lists them as `(input segment, fragment)`, in the order they are appended.
    pub fn extra_fragments(&self, module: usize) -> Vec<(usize, &Fragment)> {
        (0..self.per_segment.len())
            .flat_map(|segment_idx| {
                self.fragments(segment_idx, module)
                    .skip(1)
                    .map(move |fragment| (segment_idx, fragment))
            })
            .collect()
    }
}

impl DataEmitInfo {
    pub fn new(input_module: &InputModule, program_info: &SplitProgramInfo) -> Result<Self> {
        enum DataSegmentAnalysis {
            FromInputInAll,
            FromInputOnlyIn(usize),
            Ranges {
                ranges: Vec<LateDataRange>,
                // symbol -> (index in ranges, offset in range)
                range_lookup: HashMap<usize, (usize, u64)>,
                base_address: u64,
            },
        }
        // Active segments are initialized in index order. Relocated data is emitted in
        // additional segments after all input segments, which is only order-preserving when the
        // input segments do not overlap in memory. wasm-ld never lets them overlap, but a
        // hand-made input may; keep overlapping segments as they are, in their slots. An active
        // segment with an unknown address may overlap any other active segment; passive
        // segments have no address at instantiation and do not affect this ordering.
        let mut active_unknown_address = None;
        let segment_extents: Vec<Option<Range<u64>>> = input_module
            .data_segments
            .iter()
            .enumerate()
            .map(|(segment_idx, segment)| match &segment.kind {
                DataKind::Active { .. } => {
                    let Some(base) = input_module.reloc_info.data_segment_addresses[segment_idx]
                    else {
                        active_unknown_address.get_or_insert(segment_idx);
                        return None;
                    };
                    Some(base..base + wasm_data_len(segment))
                }
                DataKind::Passive => None,
            })
            .collect();
        let overlaps_other_segment = |segment_idx: usize| -> bool {
            let Some(extent) = &segment_extents[segment_idx] else {
                return false;
            };
            segment_extents
                .iter()
                .enumerate()
                .any(|(other_idx, other)| {
                    other_idx != segment_idx
                        && other.as_ref().is_some_and(|other| {
                            extent.start < other.end && other.start < extent.end
                        })
                })
        };
        if let Some(&unknown_active) = active_unknown_address.as_ref() {
            warn!("Found an active data segment {unknown_active} whose base address can not be determined. \
                Putting all active memory segments in main.");
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
                    let Some(extent) = &segment_extents[segment_idx] else {
                        let invalid_value = match offset_expr.get_operators_reader().read().unwrap() {
                            wasmparser::Operator::I32Const { value } => i64::from(value),
                            wasmparser::Operator::I64Const { value } => value,
                            op => {
                                warn!("Non-constant operator {op:?} found to specify a segment #{segment_idx}'s base address.");
                                return DataSegmentAnalysis::FromInputOnlyIn(MAIN_MODULE);
                            }
                        };
                        warn!("Invalid base address ({invalid_value}) found as segment #{segment_idx}'s base address");
                        return DataSegmentAnalysis::FromInputOnlyIn(MAIN_MODULE);
                    };
                    if active_unknown_address.is_some() {
                        return DataSegmentAnalysis::FromInputOnlyIn(MAIN_MODULE);
                    }
                    if overlaps_other_segment(segment_idx) {
                        warn!("Data segment {segment_idx} overlaps another data segment in memory. Putting it into main.");
                        return DataSegmentAnalysis::FromInputOnlyIn(MAIN_MODULE);
                    }
                    let address = extent.start;
                    DataSegmentAnalysis::Ranges {
                        ranges: vec![],
                        range_lookup: HashMap::new(),
                        base_address: address,
                    }
                }
            })
            .collect::<Vec<_>>();

        // Now go through the data symbols of all output modules.
        //
        // wasm-ld can place several symbols onto the same bytes: identical constants are
        // deduplicated and strings are tail-merged when optimizing. These symbols do not
        // necessarily end up in the same output module. Relocating the symbols of each module
        // on its own would copy the shared bytes once per module, which makes the segment longer
        // than its input and forces the *whole* segment into the main module (see below).
        // Instead, the symbols of all modules are sorted by input position and overlapping
        // symbols are merged into one range. A range needed by more than one output module is
        // emitted from a module that is loaded whenever any of them is: the chunk shared by
        // all the splits requiring it, or else the main module. The other modules refer to
        // its address.
        let module_by_identifier: HashMap<&SplitModuleIdentifier, usize> = program_info
            .output_modules
            .iter()
            .enumerate()
            .map(|(index, (identifier, _))| (identifier, index))
            .collect();
        // The output module to emit a range from that is required by `needed_by`.
        let placement_module = |needed_by: &SplitModuleIdentifier| -> usize {
            if let Some(&index) = module_by_identifier.get(needed_by) {
                return index;
            }
            let SplitModuleIdentifier::Chunk(needed_by) = needed_by else {
                return MAIN_MODULE;
            };
            // No chunk is shared by exactly these splits: use the smallest one that is loaded
            // by all of them, if any.
            program_info
                .output_modules
                .iter()
                .enumerate()
                .filter_map(|(index, (identifier, _))| match identifier {
                    SplitModuleIdentifier::Chunk(splits) if splits.is_superset(needed_by) => {
                        Some((splits.len(), index))
                    }
                    _ => None,
                })
                .min()
                .map_or(MAIN_MODULE, |(_, index)| index)
        };
        let mut included_symbols = Vec::new();
        for (module_index, (_, module)) in program_info.output_modules.iter().enumerate() {
            for symbol in module.included_symbols.iter() {
                let DepNode::DataSymbol(symbol_index) = *symbol else {
                    continue;
                };
                let SymbolInfo::Data {
                    symbol: Some(def_data),
                    ..
                } = input_module.reloc_info.symbols[symbol_index]
                else {
                    // Undefined data symbol: nothing defines it, so there is
                    // no definition to place and no address to relocate.
                    // The linker already resolved every reference to it
                    // (to 0 under --allow-undefined), and `reloc_value`
                    // leaves such references untouched, so the symbol
                    // simply has no place in the emit state. Toolchains
                    // produce these in the wild, e.g. rustc incremental
                    // builds whose reused objects still reference renamed
                    // promoted anonymous globals (rust-lang/rust#81280).
                    if let SymbolInfo::Data { name, .. } =
                        input_module.reloc_info.symbols[symbol_index]
                    {
                        trace!("undefined data symbol {name:?} in included set; references keep their linker value");
                    }
                    continue;
                };
                if def_data.size == 0 {
                    // We don't care about zero-sized symbols.
                    // There are some prominent examples, specifically __heap_base, that lead to a zero-sized
                    // data symbol in the output. For this example specifically, it sometimes leads to problems
                    // since it is often outside the range of data defined via segments in the input.
                    continue;
                }
                let segment_index = def_data.index as usize;
                let DataSegmentAnalysis::Ranges { .. } = &per_segment[segment_index] else {
                    // Only relocate if in active range
                    continue;
                };
                included_symbols.push((module_index, symbol_index, def_data));
            }
        }
        // Symbols starting at the same offset are ordered longest first, so that a range always
        // begins with the symbol that extends furthest. All keys are unique by the inclusion of
        // the symbol index.
        included_symbols.sort_unstable_by_key(|&(_, sym_index, ref def_data)| {
            (
                def_data.index,
                def_data.offset,
                std::cmp::Reverse(def_data.size),
                sym_index,
            )
        });
        for (module_index, symbol_index, def_data) in included_symbols {
            let segment_index = def_data.index as usize;
            let DataSegmentAnalysis::Ranges {
                ranges,
                range_lookup,
                ..
            } = &mut per_segment[segment_index]
            else {
                // filtered above. Passive and TLS segments are copied to every module.
                unreachable!(
                    "data symbol in passive range should not have gotted included in this pass"
                );
            };
            let data_len = u64::from(def_data.size);
            let data_offset = u64::from(def_data.offset);
            let data_range = data_offset..data_offset + data_len;

            let in_segment = &input_module.data_segments[segment_index];
            if data_range.end > wasm_data_len(in_segment) {
                unreachable!(
                    "Found data symbol {:?} that extends past the input module's data \
                    bytes: {data_range:?} not in range for data segment of length {}",
                    input_module.reloc_info.symbols[symbol_index],
                    in_segment.data.len()
                );
            }

            let segment_align = 1u64 << input_module.reloc_info.segments[segment_index].alignment;

            let mut data_align = segment_align;
            if data_offset != 0 {
                // TODO: .isolate_least_significant_one()
                data_align = data_align.min(1 << data_offset.trailing_zeros());
            }
            debug_assert!(data_len != 0, "zero-sized symbols handled previously");
            data_align = data_align.min(1 << data_len.trailing_zeros());

            let mut has_merged = false;
            let range_idx = ranges.len();
            if let Some(back) = ranges.last_mut() {
                let has_overlap = data_offset < back.input_range.end;
                if has_overlap {
                    // we sorted before ingesting, hence the existing range starts earlier
                    debug_assert!(
                        back.input_range.start <= data_offset,
                        "overlapping range goes backwards"
                    );
                    let range_offset = data_offset - back.input_range.start;
                    let range_idx = range_idx - 1; // back of ranges

                    has_merged = true;
                    // Contained symbols (struct fields or merged string suffixes) cannot be
                    // stricter aligned than their container. Partial overlaps raise the
                    // alignment; the range start must remain a multiple of it (checked below).
                    let contained = data_range.end <= back.input_range.end;
                    if !contained {
                        warn!(
                            "data symbol {symbol_index} partially overlaps the symbols before it in segment {segment_index} ({:?} vs {:?}); the linker is not expected to produce this",
                            data_range, back.input_range
                        );
                        back.input_range.end = data_range.end;
                        back.data_align = back.data_align.max(data_align);
                    }
                    // Track the modules actually needing the range, not the module chosen to
                    // emit it: the latter may be a superset chunk, which would rule out an exact
                    // chunk for a later, larger requirement. Record every owner, even one that
                    // happens to be the module currently chosen: its splits are needed too.
                    back.needed_by
                        .also_in(&program_info.output_modules[module_index].0);
                    range_lookup.insert(symbol_index, (range_idx, range_offset));
                }
            }
            if !has_merged {
                ranges.push(LateDataRange {
                    input_range: data_range,
                    needed_by: program_info.output_modules[module_index].0.clone(),
                    in_module: module_index,
                    data_align,
                    segment_offset: u64::MAX, // filled in later
                });
                range_lookup.insert(symbol_index, (range_idx, 0));
            }
        }
        // Derive placement from `needed_by` once all owners are known.
        for segment in &mut per_segment {
            if let DataSegmentAnalysis::Ranges { ranges, .. } = segment {
                for range in ranges {
                    let placement = placement_module(&range.needed_by);
                    if placement != range.in_module {
                        trace!(
                            "data range {:?} placed in module {placement}",
                            range.input_range
                        );
                    }
                    range.in_module = placement;
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
                    let input_len = wasm_data_len(&input_module.data_segments[segment_index]);
                    // Partial overlaps must leave the range start aligned. Keep the segment
                    // whole in main if they do not, or if the relocated layout is overlong:
                    // other active segments cannot be moved and must not be overwritten.
                    if let Some(range) = ranges.iter().find(|r| !r.input_range.start.is_multiple_of(r.data_align)) {
                        warn!(
                            "Data segment {segment_index}: partially overlapping symbols at {:?} need an alignment of {} that their start does not have. Putting it into main.",
                            range.input_range, range.data_align
                        );
                        DataSegmentEmitInfo::FromInputOnlyIn(MAIN_MODULE)
                    } else {
                        // check that range_lookup completely covers the (non-zero) data segment?
                        // Otherwise there is non-relocated data, which most likely indicates an error.
                        // There might be data symbols that are not included/depended upon anywhere though.
                        // Some gaps will exist, introduced by padding for alignment! This padding should be zeroed.
                        // So for the moment, don't bother with this sanity analysis.
                        let layout = layout_ranges(&mut ranges);
                        if layout.segment_len > input_len {
                            trace!("{ranges:?}");
                            warn!("Overlong segment {segment_index} after relocation, putting it in main module.");
                            DataSegmentEmitInfo::FromInputOnlyIn(MAIN_MODULE)
                        } else {
                            DataSegmentEmitInfo::Ranges {
                                ranges,
                                base_address,
                                range_lookup,
                                layout,
                            }
                        }
                    }
                }
            })
            .collect::<Vec<_>>();
        Ok(Self { per_segment })
    }
    pub fn find_relocated_address(
        &self,
        symbol_index: usize,
        data: &DefinedDataSymbol,
    ) -> Result<Option<u64>, ()> {
        if data.size == 0 {
            // zero-sized symbols are not relocated
            return Ok(None);
        }
        let segment_idx = data.index as usize;
        let DataSegmentEmitInfo::Ranges {
            ranges,
            base_address,
            range_lookup,
            ..
        } = &self.per_segment[segment_idx]
        else {
            // If just copied, then its not relocated
            return Ok(None);
        };
        let Some(&(range_index, offset_in_range)) = range_lookup.get(&symbol_index) else {
            return Err(());
        };
        let range = &ranges[range_index];
        Ok(Some(base_address + range.segment_offset + offset_in_range))
    }

    fn emits_data_in(&self, input_module: &InputModule, output_module_index: usize) -> bool {
        self.per_segment
            .iter()
            .zip(&input_module.data_segments)
            .any(|(emit_info, segment)| match emit_info {
                DataSegmentEmitInfo::FromInputInAll => wasm_data_len(segment) > 0,
                DataSegmentEmitInfo::FromInputOnlyIn(module) => {
                    *module == output_module_index && wasm_data_len(segment) > 0
                }
                DataSegmentEmitInfo::Ranges { layout, .. } => layout
                    .fragments
                    .iter()
                    .any(|(frag_module, _)| *frag_module == output_module_index),
            })
    }
}

pub fn module_defines_anything(
    input_module: &InputModule,
    info: &OutputModuleInfo,
    data: &DataEmitInfo,
    output_module_index: usize,
) -> bool {
    let defines_function = info.included_symbols.iter().any(|dep| match dep {
        DepNode::Function(id) => *id >= input_module.imported_funcs.len(),
        _ => false,
    });
    defines_function || data.emits_data_in(input_module, output_module_index)
}
