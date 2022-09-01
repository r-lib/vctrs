# can customise subscript errors

    Code
      (expect_error(with_tibble_cols(vec_as_subscript(env())), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` has the wrong type `environment`.
      i It must be logical, numeric, or character.

---

    Code
      (expect_error(with_dm_tables(vec_as_subscript(env())), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must extract tables with a valid subscript vector.
      x Subscript `foo(bar)` has the wrong type `environment`.
      i It must be logical, numeric, or character.

# vec_as_subscript() checks dimensionality

    Code
      (expect_error(vec_as_subscript(matrix(TRUE, nrow = 1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not a matrix.
    Code
      (expect_error(vec_as_subscript(array(TRUE, dim = c(1, 1, 1))), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not an array.
    Code
      (expect_error(with_tibble_rows(vec_as_subscript(matrix(TRUE, nrow = 1))),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must remove rows with a valid subscript vector.
      x Subscript `foo(bar)` must be a simple vector, not a matrix.

# vec_as_subscript() forbids subscript types

    Code
      vec_as_subscript(1L, logical = "error", numeric = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `integer`.
      i It must be character.

---

    Code
      vec_as_subscript("foo", logical = "error", character = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `character`.
      i It must be numeric.

---

    Code
      vec_as_subscript(TRUE, logical = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

---

    Code
      vec_as_subscript("foo", character = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `character`.
      i It must be logical or numeric.

---

    Code
      vec_as_subscript(NULL, numeric = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `NULL`.
      i It must be logical or character.

---

    Code
      vec_as_subscript(quote(foo), character = "error")
    Condition
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `symbol`.
      i It must be logical or numeric.

# vec_as_subscript2() forbids subscript types

    Code
      vec_as_subscript2(1L, numeric = "error")
    Condition
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `integer`.
      i It must be character.

---

    Code
      vec_as_subscript2("foo", character = "error")
    Condition
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `character`.
      i It must be numeric.

---

    Code
      vec_as_subscript2(TRUE)
    Condition
      Error:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

# vec_as_subscript2() retains the call when throwing vec_as_subscript() errors (#1605)

    Code
      vec_as_subscript2(1L, numeric = "error", call = call("foo"))
    Condition
      Error in `foo()`:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `integer`.
      i It must be character.

---

    Code
      vec_as_subscript2(1.5, call = call("foo"))
    Condition
      Error in `foo()`:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.

# vec_as_subscript2() retains the call when erroring on logical input (#1605)

    Code
      vec_as_subscript2(TRUE, call = call("foo"))
    Condition
      Error in `foo()`:
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

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
      ! Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

# lossy cast errors for scalar subscripts work (#1606)

    Code
      vec_as_subscript2(1.5)
    Condition
      Error:
      ! Must extract element with a single valid subscript.
      x Can't convert from <double> to <integer> due to loss of precision.

