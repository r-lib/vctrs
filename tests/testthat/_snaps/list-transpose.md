# empty `x` requires both `ptype` and `size`

    Code
      list_transpose(list())
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(), size = 1)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element type. Please supply `ptype` directly.

---

    Code
      list_transpose(list(), ptype = integer())
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

# can recover original type and size with manual `ptype` and `size`

    Code
      list_transpose(out)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

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

# recycles inputs to common size before transposing

    Code
      x <- list(1:2, 3:5)
      list_transpose(x)
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `x[[1]]` (size 2) to match `x[[2]]` (size 3).

# respects `size`

    Code
      list_transpose(list(1:2), size = 3)
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `list(1:2)[[1]]` (size 2) to size 3.

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

# doesn't allow `NULL` elements

    Code
      list_transpose(list(1:4, NULL, 5:8))
    Condition
      Error in `list_transpose()`:
      ! `list(1:4, NULL, 5:8)[[2]]` must be a vector, not `NULL`.

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

# `x` being a list subclass can't affect the transposition

    Code
      vec_cast(list(null), to = x)
    Condition
      Error:
      ! Can't convert `list(null)` <list> to <my_list>.

# `x` being a <list_of> doesn't affect the transposition

    Code
      list_transpose(x, size = 0)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element type. Please supply `ptype` directly.

---

    Code
      list_transpose(x)
    Condition
      Error in `list_transpose()`:
      ! `x[[1]]` must be a vector, not `NULL`.

# `null` must be a vector

    Code
      list_transpose(x, null = lm(1 ~ 1))
    Condition
      Error in `list_transpose()`:
      ! `null` must be a vector, not a <lm> object.

---

    Code
      list_transpose(x, null = lm(1 ~ 1))
    Condition
      Error in `list_transpose()`:
      ! `null` must be a vector, not a <lm> object.

# `null` is cast to common type (not part of common type determination)

    Code
      list_transpose(x, null = "x")
    Condition
      Error in `list_transpose()`:
      ! Can't convert `null` <character> to <integer>.

---

    Code
      list_transpose(x, null = "x", ptype = double())
    Condition
      Error in `list_transpose()`:
      ! Can't convert `null` <character> to <double>.

---

    Code
      list_transpose(x, null = "x")
    Condition
      Error in `list_transpose()`:
      ! Can't convert `null` <character> to <integer>.

---

    Code
      list_transpose(x, null = "x", ptype = double())
    Condition
      Error in `list_transpose()`:
      ! Can't convert `null` <character> to <double>.

# `null` is recycled to common size (not part of common size determination)

    Code
      list_transpose(x, null = 7:9)
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 3) to size 2.

# `null` size 0 behavior

    Code
      list_transpose(list(), null = double())
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(NULL), null = double())
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(1, 2), null = double())
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 0) to size 1.

---

    Code
      list_transpose(list(1, 2, NULL), null = double())
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 0) to size 1.

# `null` size 1 behavior

    Code
      list_transpose(list(), null = 3)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(NULL), null = 3)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

# `null` size >1 behavior

    Code
      list_transpose(list(), null = 3:4)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(NULL), null = 3:4)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(), null = 3:4, size = 0, ptype = integer())
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 2) to size 0.

---

    Code
      list_transpose(list(NULL), null = 3:4, size = 0, ptype = integer())
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 2) to size 0.

---

    Code
      list_transpose(list(1, 2), null = 3:4)
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 2) to size 1.

---

    Code
      list_transpose(list(1, 2, NULL), null = 3:4)
    Condition
      Error in `list_transpose()`:
      ! Can't recycle `null` (size 2) to size 1.

# `null` can't influence the output type in the empty `list()` case

    Code
      list_transpose(list(), null = 1L)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(), null = 1L, size = 0)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element type. Please supply `ptype` directly.

---

    Code
      list_transpose(list(), null = 1L, size = 1)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element type. Please supply `ptype` directly.

# `null` can't influence the output type in the only `NULL` case

    Code
      list_transpose(list(NULL, NULL), null = 1L)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element size. Please supply `size` directly.

---

    Code
      list_transpose(list(NULL, NULL), null = 1L, size = 1)
    Condition
      Error in `list_transpose()`:
      ! Can't automatically infer the element type. Please supply `ptype` directly.

