# `x` is validated

    Code
      list_of_transpose(1)
    Condition
      Error in `list_of_transpose()`:
      ! `1` must be a `<list_of>`, not the number 1.

---

    Code
      list_of_transpose(1, x_arg = "x", error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! `x` must be a `<list_of>`, not the number 1.

# `x` must be a fully specified list of

    Code
      x <- list_of(.ptype = integer(), .size = zap())
      list_of_transpose(x)
    Condition
      Error in `list_of_transpose()`:
      ! `x` must be a fully specified `<list_of>`.
      i `size` is not specified.

---

    Code
      x <- list_of(.ptype = zap(), .size = 1)
      list_of_transpose(x)
    Condition
      Error in `list_of_transpose()`:
      ! `x` must be a fully specified `<list_of>`.
      i `ptype` is not specified.

# `...` must be empty

    Code
      list_of_transpose(list_of2(1), 2)
    Condition
      Error in `list_of_transpose()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

# doesn't allow `NULL` elements

    Code
      list_of_transpose(list_of2(1:4, NULL, 5:8))
    Condition
      Error in `list_of_transpose()`:
      ! `list_of2(1:4, NULL, 5:8)` can't contain `NULL` values.

