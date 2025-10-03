//! Single-use range map
//!
//! Items can be inserted once, never removed or replaced and then be queried.
//! 0-length ranges are not supported.
//! Used to keep track of data relocations.
use std::{collections::BTreeMap, ops::Range};

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
    pub fn insert(&mut self, range: Range<Key>, value: V) {
        assert!(!range.is_empty(), "empty ranges are not supported");
        if let Some(lap) = self.rm.range(..range.end).last() {
            assert!(
                lap.1.end <= range.start,
                "overlapping ranges or replacements are not supported"
            );
        }
        self.rm.insert(
            range.start,
            Value {
                end: range.end,
                value,
            },
        );
    }
}
