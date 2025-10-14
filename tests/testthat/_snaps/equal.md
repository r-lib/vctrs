# throws error for unsuported type

    Code
      vec_equal(expression(x), expression(x))
    Condition <vctrs_error_scalar_type>
      Error in `vec_equal()`:
      ! `x` must be a vector, not an expression vector.
      i Read our FAQ about scalar types (`?faq_error_scalar_type`) to learn more.

# `na_equal` is validated

    Code
      vec_equal(1, 1, na_equal = 1)
    Condition
      Error in `vec_equal()`:
      ! `na_equal` must be `TRUE` or `FALSE`.

---

    Code
      vec_equal(1, 1, na_equal = c(TRUE, FALSE))
    Condition
      Error in `vec_equal()`:
      ! `na_equal` must be `TRUE` or `FALSE`.

# can't supply NA as `na_equal`

    Code
      vec_equal(NA, NA, na_equal = NA)
    Condition
      Error in `vec_equal()`:
      ! `na_equal` must be `TRUE` or `FALSE`.

