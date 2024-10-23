# error is thrown when comparing complexes (#1655)

    Code
      (expect_error(vec_compare(complex(), complex())))
    Output
      <error/rlang_error>
      Error in `vec_compare()`:
      ! Can't compare complexes.

# `na_equal` is validated

    Code
      (expect_error(vec_compare(1, 1, na_equal = 1)))
    Output
      <error/rlang_error>
      Error in `vec_compare()`:
      ! `na_equal` must be `TRUE` or `FALSE`, not the number 1.
    Code
      (expect_error(vec_compare(1, 1, na_equal = c(TRUE, FALSE))))
    Output
      <error/rlang_error>
      Error in `vec_compare()`:
      ! `na_equal` must be `TRUE` or `FALSE`, not a logical vector.

# can't supply NA as `na_equal`

    Code
      vec_compare(NA, NA, na_equal = NA)
    Condition
      Error in `vec_compare()`:
      ! `na_equal` must be `TRUE` or `FALSE`, not `NA`.

