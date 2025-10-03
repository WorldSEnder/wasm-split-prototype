use std::ops::Range;

/// Find a subrange such that both predicates are true for all elements in the subrange.
pub fn find_subrange<T>(
    slice: &[T],
    eventually_true: impl Fn(&T) -> bool,
    eventually_false: impl Fn(&T) -> bool,
) -> Range<usize> {
    let start = slice.partition_point(|t| !eventually_true(t));
    let end = slice.partition_point(eventually_false);
    start..end
}

/// Find the range of all items that completely contain the given range
pub fn find_by_range<T, U: Ord>(
    slice: &[T],
    range: &Range<U>,
    get_range: impl Fn(&T) -> Range<U>,
) -> Range<usize> {
    find_subrange(
        slice,
        |item| range.end <= get_range(item).end,
        |item| get_range(item).start <= range.start,
    )
}
