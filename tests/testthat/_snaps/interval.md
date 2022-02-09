# incomplete intervals can cause an error

    Code
      (expect_error(vec_locate_interval_merge_groups(NA, NA, incomplete = "error")))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `start` and `end` can't contain missing values.
    Code
      (expect_error(vec_locate_interval_merge_groups(1, NA, incomplete = "error")))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `start` and `end` can't contain missing values.
    Code
      (expect_error(vec_locate_interval_merge_groups(NA, 1, incomplete = "error")))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `start` and `end` can't contain missing values.

# empty and invalid intervals cause an error

    Code
      (expect_error(vec_locate_interval_merge_groups(1, 1)))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `start` must be less than `end`.
    Code
      (expect_error(vec_locate_interval_merge_groups(1, 0)))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `start` must be less than `end`.

# common type is taken

    Code
      (expect_error(vec_locate_interval_merge_groups(1, "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `start` <double> and `end` <character>.

