//! Merge small shared chunks (`Options::min_chunk_size`).
//!
//! The painter gives every distinct set of split modules that share code its own
//! chunk. With ~130 split points that can be thousands of chunks, most of them a
//! few hundred bytes, and loading one split module fetches and instantiates
//! hundreds of them.
//!
//! Merging chunk `A` (needed by splits `S_A`) with chunk `B` gives one chunk
//! needed by `S_A ∪ S_B`. That stays correct: every split that needed either
//! still loads the merged chunk before it runs. The price is bytes a split now
//! downloads without using: `size(A) * |S_B \ S_A| + size(B) * |S_A \ S_B|`.
//! The main module counts as a chunk needed by every split, whose own bytes are
//! never extra (they are always loaded already).
//!
//! Greedy: take the smallest chunk under `min_size`, merge it where that price is
//! lowest, repeat until no chunk is under `min_size`.

use std::collections::{BTreeSet, HashMap};

use crate::dep_graph::DepNode;
use crate::read::InputModule;
use crate::split_point::{OutputModuleInfo, SplitModuleIdentifier};

/// Emitted bytes do not track input bytes exactly, but code dominates both.
fn estimated_size(module: &InputModule, info: &OutputModuleInfo) -> usize {
    let imported = module.imported_funcs.len();
    info.included_symbols
        .iter()
        .map(|node| match *node {
            DepNode::Function(func) if func >= imported => {
                let range = module.defined_funcs[func - imported].body.range();
                (range.end - range.start) as usize
            }
            _ => 0,
        })
        .sum()
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Bits(Vec<u64>);

impl Bits {
    fn new(len: usize) -> Self {
        Bits(vec![0; len.div_ceil(64)])
    }
    fn set(&mut self, i: usize) {
        self.0[i / 64] |= 1 << (i % 64);
    }
    fn union(&self, other: &Bits) -> Bits {
        Bits(self.0.iter().zip(&other.0).map(|(a, b)| a | b).collect())
    }
    /// |self \ other|
    fn minus_count(&self, other: &Bits) -> usize {
        self.0
            .iter()
            .zip(&other.0)
            .map(|(a, b)| (a & !b).count_ones() as usize)
            .sum()
    }
}

struct Group {
    splits: Bits,
    size: usize,
    is_main: bool,
    members: Vec<SplitModuleIdentifier>,
}

/// Greedily merges every group under `min_size` into the group where it costs
/// the fewest extra downloaded bytes. `groups[0]` must be the main module.
/// Returns which groups survive; merged groups are left empty.
fn merge_groups(groups: &mut [Group], min_size: usize) -> Vec<bool> {
    let mut alive: Vec<bool> = vec![true; groups.len()];
    let next_small = |groups: &[Group], alive: &[bool]| {
        (1..groups.len())
            .filter(|&g| alive[g] && groups[g].size < min_size)
            .min_by_key(|&g| (groups[g].size, g))
    };
    while let Some(small) = next_small(groups, &alive) {
        let cost = |target: usize| {
            let (a, b) = (&groups[small], &groups[target]);
            let extra_b = if b.is_main {
                0
            } else {
                b.size * a.splits.minus_count(&b.splits)
            };
            (a.size * b.splits.minus_count(&a.splits) + extra_b, b.size)
        };
        let target = (0..groups.len())
            .filter(|&g| g != small && alive[g])
            .min_by_key(|&g| (cost(g), g))
            .expect("the main module is always a target");
        let taken = std::mem::replace(
            &mut groups[small],
            Group {
                splits: Bits::new(0),
                size: 0,
                is_main: false,
                members: vec![],
            },
        );
        alive[small] = false;
        let into = &mut groups[target];
        into.splits = into.splits.union(&taken.splits);
        into.size += taken.size;
        into.members.extend(taken.members);
    }
    alive
}

pub fn merge_small_chunks(
    module: &InputModule,
    contents: &mut HashMap<SplitModuleIdentifier, OutputModuleInfo>,
    min_size: usize,
) {
    if min_size == 0 {
        return;
    }
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

    let mut all = Bits::new(names.len());
    (0..names.len()).for_each(|i| all.set(i));
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
        let mut splits = Bits::new(names.len());
        set.iter().for_each(|n| splits.set(index[n.as_str()]));
        groups.push(Group {
            splits,
            size: estimated_size(module, info),
            is_main: false,
            members: vec![id.clone()],
        });
    }
    let chunks_before = groups.len() - 1;
    let alive = merge_groups(&mut groups, min_size);

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
            let set: BTreeSet<String> = (0..names.len())
                .filter(|&i| group.splits.0[i / 64] & (1 << (i % 64)) != 0)
                .map(|i| names[i].clone())
                .collect();
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
    tracing::info!("merged chunks under {min_size} B: {chunks_before} -> {chunks_after}");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn group(splits: &[usize], size: usize) -> Group {
        let mut bits = Bits::new(4);
        splits.iter().for_each(|&i| bits.set(i));
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
    fn keeps_chunks_at_or_above_min_size() {
        let mut groups = vec![main_group(), group(&[0, 1], 100), group(&[2, 3], 200)];
        assert_eq!(merge_groups(&mut groups, 100), vec![true, true, true]);
    }

    #[test]
    fn merges_into_chunk_with_fewest_extra_bytes() {
        // {0,1} is a subset of {0,1,2}: merging there costs 10 * 1 + 1000 * 0,
        // cheaper than main (10 * 2) or {2,3} (10 * 2 + 1000 * 2).
        let mut groups = vec![
            main_group(),
            group(&[0, 1], 10),
            group(&[0, 1, 2], 1000),
            group(&[2, 3], 1000),
        ];
        assert_eq!(
            merge_groups(&mut groups, 100),
            vec![true, false, true, true]
        );
        assert_eq!(groups[2].size, 1010);
        assert_eq!(groups[2].splits, group(&[0, 1, 2], 0).splits);
    }

    #[test]
    fn falls_back_to_main_module() {
        // Merging into the other chunk would download 1000 extra bytes in split 0
        // and 1; main only costs this chunk's 10 bytes in splits 2 and 3.
        let mut groups = vec![main_group(), group(&[0, 1], 10), group(&[2, 3], 1000)];
        assert_eq!(merge_groups(&mut groups, 100), vec![true, false, true]);
        assert_eq!(groups[0].size, 10);
    }

    #[test]
    fn merges_repeatedly_until_no_chunk_is_small() {
        let mut groups = vec![main_group(), group(&[0, 1], 40), group(&[0, 1, 2], 40)];
        let alive = merge_groups(&mut groups, 100);
        // The two chunks merge (80 B, still small), then go into main.
        assert_eq!(alive, vec![true, false, false]);
        assert_eq!(groups[0].size, 80);
    }
}
