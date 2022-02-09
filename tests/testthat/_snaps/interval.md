# `missing` is validated

    Code
      (expect_error(vec_locate_interval_merge_groups(1, 2, missing = "s")))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `missing` must be either "merge" or "drop".

---

    Code
      (expect_error(vec_locate_interval_merge_groups(1, 2, missing = c("merge",
        "drop"))))
    Output
      <error/rlang_error>
      Error in `vec_locate_interval_merge_groups()`:
      ! `missing` must be a string.

# common type is taken

    Code
      (expect_error(vec_locate_interval_merge_groups(1, "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `start` <double> and `end` <character>.

