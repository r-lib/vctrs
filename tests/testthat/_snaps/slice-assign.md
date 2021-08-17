# `vec_assign()` requires recyclable value

    Code
      vec_assign(1:3, 1:3, 1:2)
    Error <vctrs_error_incompatible_size>
      Can't recycle input of size 2 to size 3.

# logical subscripts must match size of indexed vector

    Code
      vec_assign(1:2, c(TRUE, FALSE, TRUE), 5)
    Error <vctrs_error_subscript_size>
      Must assign to elements with a valid subscript vector.
      i Logical subscripts must match the size of the indexed input.
      x Input has size 2 but subscript has size 3.

---

    Code
      vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ])
    Error <vctrs_error_subscript_size>
      Must assign to elements with a valid subscript vector.
      i Logical subscripts must match the size of the indexed input.
      x Input has size 32 but subscript has size 2.

# must assign with proper negative locations

    Code
      vec_assign(1:3, c(-1, 1), 1:2)
    Error <vctrs_error_subscript_type>
      Must assign to elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript has a positive value at location 2.

---

    Code
      vec_assign(1:3, c(-1, NA), 1:2)
    Error <vctrs_error_subscript_type>
      Must assign to elements with a valid subscript vector.
      x Negative locations can't have missing values.
      i Subscript has a missing value at location 2.

# `vec_assign()` error args can be overridden

    Code
      vec_assign(1:2, 1L, "x", x_arg = "foo", value_arg = "bar")
    Error <vctrs_error_incompatible_type>
      Can't convert `bar` <character> to match type of `foo` <integer>.

---

    Code
      vec_assign(1:2, 1L, 1:2, value_arg = "bar")
    Error <vctrs_error_incompatible_size>
      Can't recycle `bar` (size 2) to size 1.

# must assign to existing elements

    Code
      vec_assign(1:3, 5, 10)
    Error <vctrs_error_subscript_oob>
      Can't assign to elements that don't exist.
      x Location 5 doesn't exist.
      i There are only 3 elements.

---

    Code
      vec_assign(1:3, "foo", 10)
    Error <simpleError>
      Can't use character names to index an unnamed vector.

---

    Code
      vec_slice(letters, -100) <- "foo"
    Error <vctrs_error_subscript_oob>
      Can't negate elements that don't exist.
      x Location 100 doesn't exist.
      i There are only 26 elements.

---

    Code
      vec_assign(set_names(letters), "foo", "bar")
    Error <vctrs_error_subscript_oob>
      Can't assign to elements that don't exist.
      x Element `foo` doesn't exist.

