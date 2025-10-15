# `x` must be a list

    Code
      list_transpose(1)
    Condition
      Error in `list_transpose()`:
      ! `1` must be a list, not the number 1.

---

    Code
      list_transpose(1, x_arg = "x", error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! `x` must be a list, not the number 1.

# `...` must be empty

    Code
      list_transpose(1, 2)
    Condition
      Error in `list_transpose()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

# no recycling is done

    Code
      list_transpose(list(1L, 2:3))
    Condition
      Error in `list_transpose()`:
      ! `list(1L, 2:3)[[2]]` must have size 1, not size 2.

# doesn't allow `NULL` elements

    Code
      list_transpose(list(1:4, NULL, 5:8))
    Condition
      Error in `list_transpose()`:
      ! `list(1:4, NULL, 5:8)[[2]]` must be a vector, not `NULL`.

# respects `size`

    Code
      list_transpose(list(1:2), size = 3)
    Condition
      Error in `list_transpose()`:
      ! `list(1:2)[[1]]` must have size 3, not size 2.

# respects `ptype`

    Code
      list_transpose(list(1, 2), ptype = character())
    Condition
      Error in `list_transpose()`:
      ! Can't convert `list(1, 2)[[1]]` <double> to <character>.

---

    Code
      list_transpose(list(1, 2), ptype = character(), x_arg = "x", error_call = quote(
        foo()))
    Condition
      Error in `foo()`:
      ! Can't convert `x[[1]]` <double> to <character>.

# doesn't allow scalar elements

    Code
      list_transpose(list(1:4, lm(1 ~ 1)))
    Condition
      Error in `list_transpose()`:
      ! `list(1:4, lm(1 ~ 1))[[2]]` must be a vector, not a <lm> object.

---

    Code
      list_transpose(list(1:4, lm(1 ~ 1)), x_arg = "x", error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! `x[[2]]` must be a vector, not a <lm> object.

