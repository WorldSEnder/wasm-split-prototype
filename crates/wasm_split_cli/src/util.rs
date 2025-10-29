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

/// Find the partition point, i.e. the first index for which the monotone function returns `false`
/// with an exponential search strategy. This always returns the same index as calling [`partition_point`].
///
/// [`partition_point`]: fn@[_]::partition_point
pub fn exponential_partition_point<T>(slice: &[T], eventually_false: impl Fn(&T) -> bool) -> usize {
    let mut test_idx = 0;
    let mut step = 1;

    // The implementiation is careful to be nice to the optimizer by making bounds checks painfully obvious.
    // That's why all index accesses and constructed slices use only `test_idx` which gets checked against
    // length at the start of the loop. The slice here is then the subslice that still needs to be searched,
    // and `skipped` contains the number of items that have been sliced off from `slice`'s start so far.
    let mut to_test = slice;
    let mut skipped = 0;
    while test_idx < to_test.len() {
        let test = eventually_false(&to_test[test_idx]);
        if test {
            // Proof that partition_point is in test_idx+1..
            to_test = &to_test[test_idx..];
            skipped += test_idx;
            test_idx = step;
            // exponential step size. Note that `test_idx` will effectively test indices 2^N-1 of the input sequentially.
            step *= 2;
        } else {
            // Proof that partition_point is in ..=test_idx. `partition_point` returns the input length
            // if the test evaluates to true for all inputs, hence we can do:
            to_test = &to_test[..test_idx];
            break;
        }
    }
    skipped + to_test.partition_point(eventually_false)
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

#[cfg(test)]
#[test]
fn test_exponential_partition() {
    #[track_caller]
    fn test_property<T>(slice: &[T], eventually_false: impl Fn(&T) -> bool) {
        // for all slices and all monotone functions, we should have
        assert_eq!(
            slice.partition_point(&eventually_false),
            exponential_partition_point(slice, &eventually_false)
        );
    }
    let v = [1, 2, 3, 3, 5, 6, 7];
    let is_small = |&x: &_| x < 5;
    test_property(&v, is_small);

    let a = [2, 4, 8];
    let is_small = |&x: &_| x < 100;
    test_property(&a, is_small);

    let a: [i32; 0] = [];
    test_property(&a, is_small);

    let s = vec![0, 1, 1, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55];
    let num = 42;
    test_property(&s, |&x| x <= num);
}
