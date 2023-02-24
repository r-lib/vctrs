# `na_equal` is validated

    Code
      vec_equal(1, 1, na_equal = 1)
    Condition
      Error in `vec_equal()`:
      ! `na_equal` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      vec_equal(1, 1, na_equal = c(TRUE, FALSE))
    Condition
      Error in `vec_equal()`:
      ! `na_equal` must be `TRUE` or `FALSE`, not a logical vector.

# can't supply NA as `na_equal`

    Code
      vec_equal(NA, NA, na_equal = NA)
    Condition
      Error in `vec_equal()`:
      ! `na_equal` must be `TRUE` or `FALSE`, not `NA`.

