# can customise subscript errors

    Code
      (expect_error(with_tibble_cols(vec_as_subscript(env())), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't rename columns with `foo(bar)`.
      x `foo(bar)` must be logical, numeric, or character, not an environment.

---

    Code
      (expect_error(with_dm_tables(vec_as_subscript(env())), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't extract tables with `foo(bar)`.
      x `foo(bar)` must be logical, numeric, or character, not an environment.

# vec_as_subscript() checks dimensionality

    Code
      (expect_error(vec_as_subscript(matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements.
      x Subscript must be a simple vector, not a matrix.
    Code
      (expect_error(vec_as_subscript(array(TRUE, dim = c(1, 1, 1))), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements.
      x Subscript must be a simple vector, not an array.
    Code
      (expect_error(with_tibble_rows(vec_as_subscript(matrix(TRUE, nrow = 1))),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't remove rows with `foo(bar)`.
      x Subscript `foo(bar)` must be a simple vector, not a matrix.

# vec_as_subscript() forbids subscript types

    Code
      vec_as_subscript(1L, logical = "error", numeric = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be character, not the number 1.

---

    Code
      vec_as_subscript("foo", logical = "error", character = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be numeric, not the string "foo".

---

    Code
      vec_as_subscript(TRUE, logical = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be numeric or character, not `TRUE`.

---

    Code
      vec_as_subscript("foo", character = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be logical or numeric, not the string "foo".

---

    Code
      vec_as_subscript(NULL, numeric = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be logical or character, not `NULL`.

---

    Code
      vec_as_subscript(quote(foo), character = "error")
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be logical or numeric, not a symbol.

# vec_as_subscript2() forbids subscript types

    Code
      vec_as_subscript2(1L, numeric = "error")
    Condition
      Error:
      ! Can't extract element.
      x Subscript must be character, not the number 1.

---

    Code
      vec_as_subscript2("foo", character = "error")
    Condition
      Error:
      ! Can't extract element.
      x Subscript must be numeric, not the string "foo".

---

    Code
      vec_as_subscript2(TRUE)
    Condition
      Error:
      ! Can't extract element.
      x Subscript must be numeric or character, not `TRUE`.

# vec_as_subscript2() retains the call when throwing vec_as_subscript() errors (#1605)

    Code
      vec_as_subscript2(1L, numeric = "error", call = call("foo"))
    Condition
      Error in `foo()`:
      ! Can't extract element.
      x Subscript must be character, not the number 1.

---

    Code
      vec_as_subscript2(1.5, call = call("foo"))
    Condition
      Error in `foo()`:
      ! Can't extract element.
      x Can't convert from <double> to <integer> due to loss of precision.

# vec_as_subscript2() retains the call when erroring on logical input (#1605)

    Code
      vec_as_subscript2(TRUE, call = call("foo"))
    Condition
      Error in `foo()`:
      ! Can't extract element.
      x Subscript must be numeric or character, not `TRUE`.

# `logical = 'cast'` is deprecated

    Code
      vec_as_subscript2(TRUE, logical = "cast")
    Condition
      Error in `vec_as_subscript2()`:
      ! `vctrs::vec_as_subscript2(logical = 'cast')` is deprecated.

---

    Code
      vec_as_subscript2(TRUE, logical = "error")
    Condition
      Error:
      ! Can't extract element.
      x Subscript must be numeric or character, not `TRUE`.

# lossy cast errors for scalar subscripts work (#1606)

    Code
      vec_as_subscript2(1.5)
    Condition
      Error:
      ! Can't extract element.
      x Can't convert from <double> to <integer> due to loss of precision.

