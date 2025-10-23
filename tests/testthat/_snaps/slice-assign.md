# assign throws error with non-vector `value`

    Code
      vec_assign(x, 1L, NULL)
    Condition
      Error in `vec_assign()`:
      ! Input must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

---

    Code
      vec_assign(x, 1L, NULL, slice_value = TRUE)
    Condition
      Error in `vec_assign()`:
      ! Input must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

---

    Code
      vec_assign(x, 1L, NULL, value_arg = "foo")
    Condition
      Error in `vec_assign()`:
      ! `foo` must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

---

    Code
      vec_assign(x, 1L, NULL, slice_value = TRUE, value_arg = "foo")
    Condition
      Error in `vec_assign()`:
      ! `foo` must be a vector, not `NULL`.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

---

    Code
      vec_assign(x, 1L, environment(), value_arg = "foo")
    Condition
      Error in `vec_assign()`:
      ! `foo` must be a vector, not an environment.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

---

    Code
      vec_assign(x, 1L, environment(), slice_value = TRUE, value_arg = "foo")
    Condition
      Error in `vec_assign()`:
      ! `foo` must be a vector, not an environment.
      i Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.

# can assign using logical index

    Code
      (expect_error(vec_assign(x, c(TRUE, FALSE, TRUE), 5), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't assign elements.
      x Logical subscript must be size 1 or 2, not 3.

---

    Code
      (expect_error(vec_assign(x, c(TRUE, FALSE, TRUE), 5, slice_value = TRUE),
      class = "vctrs_error_subscript_size"))
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't assign elements.
      x Logical subscript must be size 1 or 2, not 3.

---

    Code
      (expect_error(vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ]), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't assign elements.
      x Logical subscript must be size 1 or 32, not 2.

---

    Code
      (expect_error(vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ], slice_value = TRUE),
      class = "vctrs_error_subscript_size"))
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't assign elements.
      x Logical subscript must be size 1 or 32, not 2.

# assign `value` size depends on `slice_value`

    Code
      vec_assign(x, c(TRUE, NA, FALSE), c(1, 2, 3))
    Condition
      Error in `vec_assign()`:
      ! Can't recycle input of size 3 to size 2.

---

    Code
      vec_assign(x, c(TRUE, NA, FALSE), c(1, 2), slice_value = TRUE)
    Condition
      Error in `vec_assign()`:
      ! Can't recycle input of size 2 to size 3.

# can use names to assign with a named object

    Code
      vec_assign(x, c("c", "a"), c(4, 5, 6))
    Condition
      Error in `vec_assign()`:
      ! Can't recycle input of size 3 to size 2.

---

    Code
      vec_assign(x, c("c", "a"), c(4, 5), slice_value = TRUE)
    Condition
      Error in `vec_assign()`:
      ! Can't recycle input of size 2 to size 3.

# `vec_assign()` requires recyclable value

    Code
      (expect_error(vec_assign(1:3, 1:2, 1:3), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `vec_assign()`:
      ! Can't recycle input of size 3 to size 2.

---

    Code
      (expect_error(vec_assign(1:3, 1:2, 1:2, slice_value = TRUE), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `vec_assign()`:
      ! Can't recycle input of size 2 to size 3.

# logical subscripts must match size of indexed vector

    Code
      (expect_error(vec_assign(1:2, c(TRUE, FALSE, TRUE), 5), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't assign elements.
      x Logical subscript must be size 1 or 2, not 3.

---

    Code
      (expect_error(vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ]), class = "vctrs_error_subscript_size")
      )
    Output
      <error/vctrs_error_subscript_size>
      Error:
      ! Can't assign elements.
      x Logical subscript must be size 1 or 32, not 2.

# must assign existing elements

    Code
      (expect_error(vec_assign(1:3, 5, 10), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't assign to elements past the end.
      i Location 5 doesn't exist.
      i There are only 3 elements.
    Code
      (expect_error(vec_assign(1:3, "foo", 10), "unnamed vector"))
    Output
      <error/rlang_error>
      Error in `vec_assign()`:
      ! Can't use character names to index an unnamed vector.
    Code
      (expect_error(vec_slice(letters, -100) <- "foo", class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't negate elements past the end.
      i Location 100 doesn't exist.
      i There are only 26 elements.
    Code
      (expect_error(vec_assign(set_names(letters), "foo", "bar"), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't assign to elements that don't exist.
      x Element `foo` doesn't exist.

# must assign with proper negative locations

    Code
      (expect_error(vec_assign(1:3, c(-1, 1), 1:2), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't assign elements.
      x Negative and positive locations can't be mixed.
      i Subscript has a positive value at location 2.
    Code
      (expect_error(vec_assign(1:3, c(-1, NA), 1:2), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't assign elements.
      x Negative locations can't have missing values.
      i Subscript has a missing value at location 2.

# `vec_assign()` error args can be overridden

    Code
      (expect_error(vec_assign(1:2, 1L, "x", x_arg = "foo", value_arg = "bar"),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error in `vec_assign()`:
      ! Can't convert `bar` <character> to match type of `foo` <integer>.
    Code
      (expect_error(vec_assign(1:2, 1L, 1:2, value_arg = "bar"), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `vec_assign()`:
      ! Can't recycle `bar` (size 2) to size 1.

