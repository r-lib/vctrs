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
      i Specify `null` to replace them.

# `x` being a list subclass can't affect the transposition

    Code
      vec_cast(list(null), to = x)
    Condition
      Error:
      ! Can't convert `list(null)` <list> to <my_list>.

# `null` must be a vector

    Code
      list_of_transpose(x, null = lm(1 ~ 1))
    Condition
      Error in `list_of_transpose()`:
      ! `null` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      list_of_transpose(x, null = lm(1 ~ 1))
    Condition
      Error in `list_of_transpose()`:
      ! `null` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

# `null` is cast to the element type

    Code
      list_of_transpose(x, null = "x")
    Condition
      Error in `list_of_transpose()`:
      ! Can't convert `null` <character> to <integer>.

---

    Code
      list_of_transpose(x, null = "x")
    Condition
      Error in `list_of_transpose()`:
      ! Can't convert `null` <character> to <integer>.

# `null` is recycled to the element size

    Code
      list_of_transpose(x, null = 7:9)
    Condition
      Error in `list_of_transpose()`:
      ! Can't recycle `null` (size 3) to size 2.

