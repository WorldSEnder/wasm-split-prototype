use std::{collections::BTreeMap, ops::Range};
//use std::collections::btree_map::Range as BRangeIter;

pub type Key = usize;

#[derive(Debug)]
struct Value<V> {
    end: Key,
    value: V,
}

#[derive(Debug)]
pub struct OffsetRangeMap<V> {
    rm: BTreeMap<Key, Value<V>>,
}

impl<V> OffsetRangeMap<V> {
    pub fn new() -> Self {
        Self {
            rm: BTreeMap::default(),
        }
    }
    pub fn get(&self, offset: &Key) -> Option<&V> {
        let range_query = ..=offset;
        let (_candidate_start, candidate_info) = self.rm.range(range_query).last()?;
        if &candidate_info.end <= offset {
            None
        } else {
            Some(&candidate_info.value)
        }
    }
    pub fn insert(&mut self, range: Range<usize>, value: V) {
        // TODO: check for collisions
        self.rm.insert(
            range.start,
            Value {
                end: range.end,
                value,
            },
        );
    }
}
