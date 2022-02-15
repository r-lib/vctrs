# `missing` is validated

    Code
      (expect_error(vec_interval_locate_groups(1, 2, missing = "s")))
    Output
      <error/rlang_error>
      Error in `vec_interval_locate_groups()`:
      ! `missing` must be either "group" or "drop".

---

    Code
      (expect_error(vec_interval_locate_groups(1, 2, missing = c("group", "drop"))))
    Output
      <error/rlang_error>
      Error in `vec_interval_locate_groups()`:
      ! `missing` must be a string.

# common type is taken

    Code
      (expect_error(vec_interval_locate_groups(1, "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `start` <double> and `end` <character>.

# `lower` and `upper` can't contain missing values

    Code
      (expect_error(vec_interval_complement(1, 2, lower = NA)))
    Output
      <error/rlang_error>
      Error in `vec_interval_complement()`:
      ! `lower` can't contain missing values.
    Code
      (expect_error(vec_interval_complement(1, 2, upper = NA)))
    Output
      <error/rlang_error>
      Error in `vec_interval_complement()`:
      ! `upper` can't contain missing values.
    Code
      start <- data_frame(x = 1, y = 2)
      end <- data_frame(x = 1, y = 3)
      (expect_error(vec_interval_complement(start, end, lower = data_frame(x = 1, y = NA)))
      )
    Output
      <error/rlang_error>
      Error in `vec_interval_complement()`:
      ! `lower` can't contain missing values.
    Code
      (expect_error(vec_interval_complement(start, end, upper = data_frame(x = 1, y = NA)))
      )
    Output
      <error/rlang_error>
      Error in `vec_interval_complement()`:
      ! `upper` can't contain missing values.

