# `vec_rep()` validates `times`

    Code
      (expect_error(my_vec_rep(1, "x"), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error in `my_vec_rep()`:
      ! Can't convert `my_times` <character> to <integer>.
    Code
      (expect_error(my_vec_rep(1, c(1, 2))))
    Output
      <error/rlang_error>
      Error in `my_vec_rep()`:
      ! `my_times` must be a single number.
    Code
      (expect_error(my_vec_rep(1, -1)))
    Output
      <error/rlang_error>
      Error in `my_vec_rep()`:
      ! `my_times` must be a positive number.
    Code
      (expect_error(my_vec_rep(1, NA_integer_)))
    Output
      <error/rlang_error>
      Error in `my_vec_rep()`:
      ! `my_times` can't be missing.

---

    Code
      my_vec_rep(1, "x")
    Condition
      Error in `my_vec_rep()`:
      ! Can't convert `my_times` <character> to <integer>.

---

    Code
      my_vec_rep(1, c(1, 2))
    Condition
      Error in `my_vec_rep()`:
      ! `my_times` must be a single number.

---

    Code
      my_vec_rep(1, -1)
    Condition
      Error in `my_vec_rep()`:
      ! `my_times` must be a positive number.

---

    Code
      my_vec_rep(1, NA_integer_)
    Condition
      Error in `my_vec_rep()`:
      ! `my_times` can't be missing.

# `vec_rep_each()` validates `times`

    Code
      (expect_error(my_vec_rep_each(1, "x"), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error in `my_vec_rep_each()`:
      ! Can't convert `my_times` <character> to <integer>.
    Code
      (expect_error(my_vec_rep_each(1, -1)))
    Output
      <error/rlang_error>
      Error in `my_vec_rep_each()`:
      ! `my_times` must be a vector of positive numbers. Location 1 is negative.
    Code
      (expect_error(my_vec_rep_each(c(1, 2), c(1, -1))))
    Output
      <error/rlang_error>
      Error in `my_vec_rep_each()`:
      ! `my_times` must be a vector of positive numbers. Location 2 is negative.
    Code
      (expect_error(my_vec_rep_each(1, NA_integer_)))
    Output
      <error/rlang_error>
      Error in `my_vec_rep_each()`:
      ! `my_times` can't be missing. Location 1 is missing.
    Code
      (expect_error(my_vec_rep_each(c(1, 2), c(1, NA_integer_))))
    Output
      <error/rlang_error>
      Error in `my_vec_rep_each()`:
      ! `my_times` can't be missing. Location 2 is missing.

---

    Code
      my_vec_rep_each(1, "x")
    Condition
      Error in `my_vec_rep_each()`:
      ! Can't convert `my_times` <character> to <integer>.

---

    Code
      my_vec_rep_each(1, -1)
    Condition
      Error in `my_vec_rep_each()`:
      ! `my_times` must be a vector of positive numbers. Location 1 is negative.

---

    Code
      my_vec_rep_each(c(1, 2), c(1, -1))
    Condition
      Error in `my_vec_rep_each()`:
      ! `my_times` must be a vector of positive numbers. Location 2 is negative.

---

    Code
      my_vec_rep_each(1, NA_integer_)
    Condition
      Error in `my_vec_rep_each()`:
      ! `my_times` can't be missing. Location 1 is missing.

---

    Code
      my_vec_rep_each(c(1, 2), c(1, NA_integer_))
    Condition
      Error in `my_vec_rep_each()`:
      ! `my_times` can't be missing. Location 2 is missing.

# `vec_rep_each()` uses recyclying errors

    Code
      (expect_error(my_vec_rep_each(1:2, 1:3), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `my_vec_rep_each()`:
      ! Can't recycle `my_times` (size 3) to size 2.

---

    Code
      my_vec_rep_each(1:2, 1:3)
    Condition
      Error in `my_vec_rep_each()`:
      ! Can't recycle `my_times` (size 3) to size 2.

# errors on scalars

    Code
      vec_unrep(environment())
    Condition
      Error in `vec_unrep()`:
      ! `x` must be a vector, not an environment.
      i Read our FAQ about scalar types (`?faq_error_scalar_type`) to learn more.

