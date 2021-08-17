# can customise subscript errors

    Code
      with_tibble_cols(vec_as_subscript(env()))
    Error <vctrs_error_subscript_type>
      Must rename columns with a valid subscript vector.
      x Subscript `foo(bar)` has the wrong type `environment`.
      i It must be logical, numeric, or character.

---

    Code
      with_dm_tables(vec_as_subscript(env()))
    Error <vctrs_error_subscript_type>
      Must extract tables with a valid subscript vector.
      x Subscript `foo(bar)` has the wrong type `environment`.
      i It must be logical, numeric, or character.

# vec_as_subscript() checks dimensionality

    Code
      vec_as_subscript(matrix(TRUE, nrow = 1))
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not a matrix.

---

    Code
      vec_as_subscript(array(TRUE, dim = c(1, 1, 1)))
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript must be a simple vector, not an array.

---

    Code
      with_tibble_rows(vec_as_subscript(matrix(TRUE, nrow = 1)))
    Error <vctrs_error_subscript_type>
      Must remove rows with a valid subscript vector.
      x Subscript `foo(bar)` must be a simple vector, not a matrix.

# vec_as_subscript() forbids subscript types

    Code
      vec_as_subscript(1L, logical = "error", numeric = "error")
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `integer`.
      i It must be character.

---

    Code
      vec_as_subscript("foo", logical = "error", character = "error")
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `character`.
      i It must be numeric.

---

    Code
      vec_as_subscript(TRUE, logical = "error")
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

---

    Code
      vec_as_subscript("foo", character = "error")
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `character`.
      i It must be logical or numeric.

---

    Code
      vec_as_subscript(NULL, numeric = "error")
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `NULL`.
      i It must be logical or character.

---

    Code
      vec_as_subscript(quote(foo), character = "error")
    Error <vctrs_error_subscript_type>
      Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `symbol`.
      i It must be logical or numeric.

# vec_as_subscript2() forbids subscript types

    Code
      vec_as_subscript2(1L, numeric = "error", logical = "error")
    Error <vctrs_error_subscript_type>
      Must extract element with a single valid subscript.
      x Subscript has the wrong type `integer`.
      i It must be character.

---

    Code
      vec_as_subscript2("foo", character = "error", logical = "error")
    Error <vctrs_error_subscript_type>
      Must extract element with a single valid subscript.
      x Subscript has the wrong type `character`.
      i It must be numeric.

---

    Code
      vec_as_subscript2(TRUE, logical = "error")
    Error <vctrs_error_subscript_type>
      Must extract element with a single valid subscript.
      x Subscript has the wrong type `logical`.
      i It must be numeric or character.

