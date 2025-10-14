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
      list_transpose(x)
    Condition
      Error in `list_transpose()`:
      ! `<list>[[1]]` must be a vector, not `NULL`.

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

# `null` participates in common type determination

    Code
      list_transpose(x, null = "x")
    Condition
      Error in `list_transpose()`:
      ! Can't combine `null` <character> and <integer>.

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
      ! Can't combine `null` <character> and <integer>.

---

    Code
      list_transpose(x, null = "x", ptype = double())
    Condition
      Error in `list_transpose()`:
      ! Can't convert `null` <character> to <double>.

# `null` must be size 1

    Code
      list_transpose(x, null = 2:3)
    Condition
      Error in `list_transpose()`:
      ! `null` must have size 1, not size 2.

---

    Code
      list_transpose(x, null = 4:5)
    Condition
      Error in `list_transpose()`:
      ! `null` must have size 1, not size 2.

---

    Code
      list_transpose(x, null = 2:3)
    Condition
      Error in `list_transpose()`:
      ! `null` must have size 1, not size 2.

# `null` can't result in recycle to size 0

    Code
      list_transpose(x, null = integer())
    Condition
      Error in `list_transpose()`:
      ! `null` must have size 1, not size 0.

# `null` influences type in the empty `list()` case

    Code
      list_transpose(list(), null = 1:2)
    Condition
      Error in `list_transpose()`:
      ! `null` must have size 1, not size 2.

