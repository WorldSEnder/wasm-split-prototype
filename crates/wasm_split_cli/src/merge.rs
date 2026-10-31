//! Merge shared chunks (`Options::cost_model`).
//!
//! The painter gives every distinct set of split modules that share code its own
//! chunk. With ~130 split points that can be thousands of chunks, most of them a
//! few hundred bytes, and loading one split module fetches and instantiates
//! hundreds of them.
//!
//! Merging chunk `A` (needed by splits `S_A`) with chunk `B` gives one chunk
//! needed by `S_A ∪ S_B`. That stays correct: every split that needed either
//! still loads the merged chunk before it runs. Whether it pays off depends on
//! how likely each chunk is to be loaded at all. A chunk needed by `n` splits is
//! loaded with probability `P(n) = 1 - (1 - p)^n` if each split loads independently
//! with probability `p`, and then costs `t + size`, `t` being the cost of one more
//! request in bytes. Merging saves
//! `P(|S_A|) (t + a) + P(|S_B|) (t + b) - P(|S_A ∪ S_B|) (t + a + b)`.
//! The main module is always loaded and needs no request of its own, so merging
//! `A` into it saves `P(|S_A|) (t + a) - a`.
//!
//! Greedy: take the smallest chunk, merge it where that saves the most, leave it
//! alone if nothing saves anything, repeat until a whole pass merges nothing.

use std::collections::{BTreeSet, HashMap};

use fixedbitset::FixedBitSet;

use crate::dep_graph::DepNode;
use crate::options::CostModel;
use crate::read::InputModule;
use crate::split_point::{OutputModuleInfo, SplitModuleIdentifier};
use crate::tracing_support::perf_span;

/// Emitted bytes do not track input bytes exactly, but code and data dominate both.
/// Data symbols can overlap (string tail merging), so this may count some bytes twice.
fn estimated_size(
    module: &InputModule,
    info: &OutputModuleInfo,
    data_sizes: &HashMap<usize, usize>,
) -> usize {
    let imported = module.imported_funcs.len();
    info.included_symbols
        .iter()
        .map(|node| match *node {
            DepNode::Function(func) if func >= imported => {
                let range = module.defined_funcs[func - imported].body.range();
                (range.end - range.start) as usize
            }
            DepNode::DataSymbol(symbol) => data_sizes.get(&symbol).copied().unwrap_or(0),
            _ => 0,
        })
        .sum()
}

struct Group {
    splits: FixedBitSet,
    size: usize,
    is_main: bool,
    members: Vec<SplitModuleIdentifier>,
}

/// [`CostModel::Expected`] with both parameters as `f64`.
struct ExpectedCost {
    request_cost: f64,
    load_probability: f64,
}

impl ExpectedCost {
    fn load_chance(&self, splits: usize) -> f64 {
        1.0 - (1.0 - self.load_probability).powi(splits as i32)
    }

    fn cost(&self, splits: usize, size: usize) -> f64 {
        self.load_chance(splits) * (self.request_cost + size as f64)
    }

    /// Expected bytes saved by merging `a` into `b`; negative if that costs more.
    fn saving(&self, a: &Group, b: &Group) -> f64 {
        let cost_a = self.cost(a.splits.count_ones(..), a.size);
        if b.is_main {
            return cost_a - a.size as f64;
        }
        let union = a.splits.union_count(&b.splits);
        cost_a + self.cost(b.splits.count_ones(..), b.size) - self.cost(union, a.size + b.size)
    }
}

/// Greedily merges groups for as long as that lowers the expected cost.
/// `groups[0]` must be the main module. Returns which groups survive; merged
/// groups are left empty.
fn merge_groups(groups: &mut [Group], cost: &ExpectedCost) -> Vec<bool> {
    let mut alive: Vec<bool> = vec![true; groups.len()];
    loop {
        let mut settled = vec![false; groups.len()];
        let mut merged_any = false;
        while let Some(small) = (1..groups.len())
            .filter(|&g| alive[g] && !settled[g])
            .min_by_key(|&g| (groups[g].size, g))
        {
            let (saving, target) = (0..groups.len())
                .filter(|&g| g != small && alive[g])
                .map(|g| (cost.saving(&groups[small], &groups[g]), g))
                // The lowest index wins a tie, so the output is reproducible.
                .max_by(|x, y| x.0.total_cmp(&y.0).then(y.1.cmp(&x.1)))
                .expect("the main module is always a target");
            if saving <= 0.0 {
                settled[small] = true;
                continue;
            }
            let taken = std::mem::replace(
                &mut groups[small],
                Group {
                    splits: FixedBitSet::new(),
                    size: 0,
                    is_main: false,
                    members: vec![],
                },
            );
            alive[small] = false;
            merged_any = true;
            let into = &mut groups[target];
            into.splits.union_with(&taken.splits);
            into.size += taken.size;
            into.members.extend(taken.members);
        }
        if !merged_any {
            return alive;
        }
    }
}

pub fn merge_chunks(
    module: &InputModule,
    contents: &mut HashMap<SplitModuleIdentifier, OutputModuleInfo>,
    cost_model: CostModel,
) {
    let CostModel::Expected {
        request_cost,
        load_probability,
    } = cost_model
    else {
        return;
    };
    let perf_span = perf_span!("merge chunks");
    let _perf_span = perf_span.enter();
    let cost = ExpectedCost {
        request_cost: request_cost as f64,
        load_probability,
    };
    let data_sizes: HashMap<usize, usize> = module
        .reloc_info
        .data_symbols
        .iter()
        .map(|data| {
            (
                data.symbol_index,
                (data.range.end - data.range.start) as usize,
            )
        })
        .collect();
    let names: Vec<String> = contents
        .keys()
        .filter_map(|id| match id {
            SplitModuleIdentifier::Split(name) => Some(name.clone()),
            _ => None,
        })
        .chain(contents.keys().flat_map(|id| match id {
            SplitModuleIdentifier::Chunk(set) => set.iter().cloned().collect(),
            _ => vec![],
        }))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    let index: HashMap<&str, usize> = names
        .iter()
        .enumerate()
        .map(|(i, n)| (n.as_str(), i))
        .collect();

    let mut all = FixedBitSet::with_capacity(names.len());
    all.insert_range(..);
    let mut groups = vec![Group {
        splits: all,
        size: 0,
        is_main: true,
        members: vec![SplitModuleIdentifier::Main],
    }];
    // Sorted, so ties break the same way on every run and the output is reproducible.
    let mut chunk_ids: Vec<&SplitModuleIdentifier> = contents
        .keys()
        .filter(|id| matches!(id, SplitModuleIdentifier::Chunk(_)))
        .collect();
    chunk_ids.sort();
    for id in chunk_ids {
        let SplitModuleIdentifier::Chunk(set) = id else {
            unreachable!()
        };
        let info = &contents[id];
        let mut splits = FixedBitSet::with_capacity(names.len());
        set.iter().for_each(|n| splits.insert(index[n.as_str()]));
        groups.push(Group {
            splits,
            size: estimated_size(module, info, &data_sizes),
            is_main: false,
            members: vec![id.clone()],
        });
    }
    let chunks_before = groups.len() - 1;
    let alive = merge_groups(&mut groups, &cost);

    // Rebuild the module map: each surviving group becomes one chunk keyed by
    // its split set, or the main module.
    let mut merged = HashMap::new();
    for (g, group) in groups.into_iter().enumerate() {
        if !alive[g] {
            continue;
        }
        let id = if group.is_main {
            SplitModuleIdentifier::Main
        } else {
            let set: BTreeSet<String> = group.splits.ones().map(|i| names[i].clone()).collect();
            // Every chunk is shared by at least two splits, so every union is too.
            debug_assert!(set.len() > 1);
            SplitModuleIdentifier::Chunk(set)
        };
        // Two groups can end with the same split set; they share one module.
        let info: &mut OutputModuleInfo = merged.entry(id).or_default();
        for member in group.members {
            if let Some(part) = contents.remove(&member) {
                info.included_symbols.extend(part.included_symbols);
            }
        }
    }
    let chunks_after = merged
        .keys()
        .filter(|id| matches!(id, SplitModuleIdentifier::Chunk(_)))
        .count();
    // Splits were never merged; carry them over unchanged.
    merged.extend(contents.drain());
    *contents = merged;
    tracing::info!("merged chunks: {chunks_before} -> {chunks_after}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const COST: ExpectedCost = ExpectedCost {
        request_cost: 100.0,
        load_probability: 0.05,
    };

    fn group(splits: &[usize], size: usize) -> Group {
        let mut bits = FixedBitSet::with_capacity(4);
        splits.iter().for_each(|&i| bits.insert(i));
        Group {
            splits: bits,
            size,
            is_main: false,
            members: vec![],
        }
    }

    fn main_group() -> Group {
        Group {
            is_main: true,
            ..group(&[0, 1, 2, 3], 0)
        }
    }

    #[test]
    fn keeps_chunks_when_merging_would_cost_more() {
        let mut groups = vec![main_group(), group(&[0, 1], 10000), group(&[2, 3], 10000)];
        assert_eq!(merge_groups(&mut groups, &COST), vec![true, true, true]);
    }

    #[test]
    fn merges_into_chunk_that_saves_the_most() {
        // {0,1} is a subset of {0,1,2}: loading the small chunk there adds one split
        // and removes a whole request, which beats main and the unrelated {2,3}.
        let mut groups = vec![
            main_group(),
            group(&[0, 1], 10),
            group(&[0, 1, 2], 1000),
            group(&[2, 3], 1000),
        ];
        assert_eq!(
            merge_groups(&mut groups, &COST),
            vec![true, false, true, true]
        );
        assert_eq!(groups[2].size, 1010);
        assert_eq!(groups[2].splits, group(&[0, 1, 2], 0).splits);
    }

    #[test]
    fn falls_back_to_main_module() {
        // Merging into {2,3} would load its 1000 bytes in splits 0 and 1; main only
        // loads this chunk's 10 bytes everywhere.
        let mut groups = vec![main_group(), group(&[0, 1], 10), group(&[2, 3], 1000)];
        assert_eq!(merge_groups(&mut groups, &COST), vec![true, false, true]);
        assert_eq!(groups[0].size, 10);
    }

    #[test]
    fn merges_repeatedly() {
        let cost = ExpectedCost {
            request_cost: 1000.0,
            load_probability: 0.05,
        };
        let mut groups = vec![main_group(), group(&[0, 1], 40), group(&[0, 1, 2], 40)];
        let alive = merge_groups(&mut groups, &cost);
        // The two chunks merge, then the 80 B result goes into main.
        assert_eq!(alive, vec![true, false, false]);
        assert_eq!(groups[0].size, 80);
    }
}
