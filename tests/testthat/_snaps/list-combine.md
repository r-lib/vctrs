# `list_combine()` works with homogeneous fallback in `default`

    Code
      list_combine(list(foobar(1), 1), indices = list(1, 2), size = 2)
    Condition
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <vctrs_foobar> and `x[[2]]` <double>.

---

    Code
      list_combine(list(foobar(1)), indices = list(1), size = 2, default = 1)
    Condition
      Error in `list_combine()`:
      ! Can't combine <vctrs_foobar> and `default` <double>.

# list_combine() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      list_combine(list(x, y), indices = list(1, 2), size = 2)
    Condition
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <vctrs_foobar> and `x[[2]]` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

---

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      list_combine(list(x, y), indices = list(1, 2), size = 2, error_call = call(
        "foo"), x_arg = "arg")
    Condition
      Error in `foo()`:
      ! Can't combine `arg[[1]]` <vctrs_foobar> and `arg[[2]]` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

---

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      default <- structure(foobar(2), attr_foo = "bar")
      list_combine(list(x), indices = list(1), size = 2, default = default)
    Condition
      Error in `list_combine()`:
      ! Can't combine <vctrs_foobar> and `default` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

---

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      default <- structure(foobar(2), attr_foo = "bar")
      list_combine(list(x), indices = list(1), size = 2, default = default,
      default_arg = "d")
    Condition
      Error in `list_combine()`:
      ! Can't combine <vctrs_foobar> and `d` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# list_combine() fails with complex foreign S4 classes

    Code
      joe <- .Counts(c(1L, 2L), name = "Joe")
      jane <- .Counts(3L, name = "Jane")
      (expect_error(list_combine(list(joe, jane), indices = list(1:2, 3), size = 3),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <vctrs_Counts> and `x[[2]]` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
    Code
      (expect_error(list_combine(list(joe, jane), indices = list(1:2, 3), size = 3,
      error_call = call("foo"), x_arg = "arg"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `foo()`:
      ! Can't combine `arg[[1]]` <vctrs_Counts> and `arg[[2]]` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# list_combine() falls back to c() if S3 method is available

    Code
      (expect_error(list_combine(list(foobar(1), foobar(2)), indices = list(c(1, 3),
      integer()), size = 2), class = "vctrs_error_subscript_oob"))
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

---

    Code
      x <- list(foobar(1:2))
      indices <- list(1:3)
      (expect_error(list_combine(x, indices = indices, size = 3)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.
    Code
      (expect_error(list_combine(x, indices = indices, size = 3, x_arg = "arg",
        error_call = call("foo"))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `foo()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 3.

# list_combine() falls back for S4 classes with a registered c() method

    Code
      (expect_error(list_combine(list(joe, 1, jane), indices = list(c(1, 2), 3, 4),
      size = 4), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <vctrs_Counts> and `x[[2]]` <double>.

# can ignore names in `list_combine()` by providing a `zap()` name-spec (#232)

    Code
      (expect_error(list_combine(list(a = c(b = 1:2)), indices = list(1:2), size = 2))
      )
    Output
      <error/rlang_error>
      Error in `list_combine()`:
      ! Can't merge the outer name `a` with a vector of length > 1.
      Please supply a `.name_spec` specification.
    Code
      (expect_error(list_combine(list(a = c(b = 1:2)), indices = list(1:2), size = 2,
      error_call = call("foo"))))
    Output
      <error/rlang_error>
      Error in `list_combine()`:
      ! Can't merge the outer name `a` with a vector of length > 1.
      Please supply a `.name_spec` specification.

---

    Code
      x <- list(a = c(b = c("a", "b")), b = 3L)
      (expect_error(list_combine(x, indices = list(1:2, 3), size = 3, name_spec = zap()),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `list_combine()`:
      ! Can't combine `x$a` <character> and `x$b` <integer>.
    Code
      x <- list(a = c(foo = 1:2), b = c(bar = ""))
      (expect_error(list_combine(x, indices = list(2:1, 3), size = 3, name_spec = zap()),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `list_combine()`:
      ! Can't combine `x$a` <integer> and `x$b` <character>.

# list_combine() fails if foreign classes are not homogeneous and there is no c() method

    Code
      list_combine(list(x), indices = list(c(1, 2)), size = 3, default = default)
    Condition
      Error in `list_combine()`:
      ! Can't combine <foo> and `default` <vctrs_foobar>.

# recycling error indices are correct even with `NULL` removal

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[3]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[3]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[3]]` (size 2) to size 3.

# `x_arg` works

    Code
      list_combine(list(1, "2"), indices = list(1, 2), size = 2, x_arg = "xs")
    Condition
      Error in `list_combine()`:
      ! Can't combine `xs[[1]]` <double> and `xs[[2]]` <character>.

---

    Code
      list_combine(list(1, 2), indices = list(1, 2, 3), size = 2, x_arg = "xs")
    Condition
      Error in `list_combine()`:
      ! `indices` must have size 2, not size 3.

# `indices_arg` works

    Code
      list_combine(list(1, 2), indices = 1, size = 2, indices_arg = "i")
    Condition
      Error in `list_combine()`:
      ! `i` must be a list, not the number 1.

---

    Code
      list_combine(list(1, 2), indices = list(1, 2, 3), size = 2, indices_arg = "i")
    Condition
      Error in `list_combine()`:
      ! `i` must have size 2, not size 3.

# `...` must be empty

    Code
      list_combine(list(1, 2), indices = list(1, 2), size = 2, "foo")
    Condition
      Error in `list_combine()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "foo"
      i Did you forget to name an argument?

# list_combine() `size` type is validated

    Code
      list_combine(list(1), indices = list(1), size = "x")
    Condition
      Error in `list_combine()`:
      ! `size` must be a scalar integer or double.

# list_combine() `indices` are validated against `size`

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements past the end.
      i Location 3 doesn't exist.
      i There are only 2 elements.

# list_combine() `default` vector check is done

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! `d` must be a vector, not a <lm> object.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! `d` must be a vector, not a <vctrs_foobar> object.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! `d` must be a vector, not a <vctrs_foobar> object.

# list_combine() `default` size check is done

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `d` (size 2) to size 1.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `d` (size 2) to size 1.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `d` (size 2) to size 1.

# list_combine() `default` is taken into account when computing `ptype`

    Code
      list_combine(list(x = 1), indices = list(1), size = 2, default = "a",
      default_arg = "d")
    Condition
      Error in `list_combine()`:
      ! Can't combine <double> and `d` <character>.

---

    Code
      list_combine(list(x = 1L), indices = list(1), size = 2, default = 1.5,
      default_arg = "d", ptype = integer())
    Condition
      Error in `list_combine()`:
      ! Can't convert from `d` <double> to <integer> due to loss of precision.
      * Locations: 1

# list_combine() `unmatched = 'error'` errors with unmatched `indices` when `size` is used

    Code
      list_combine(list(1, 1), indices = list(1, 1), size = 2, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      list_combine(list(1, 1), indices = list(1, NA), size = 2, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      list_combine(list(1:9, 1:9), indices = list(c(TRUE, FALSE, NA, TRUE, FALSE, NA,
        TRUE, FALSE, NA), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)),
      size = 9, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Locations 5, 6, 8, and 9 are unmatched.

---

    Code
      list_combine(list(1, 3), indices = list(1, 3), size = 3, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      list_combine(list(1, 1), indices = list(c(TRUE, FALSE), c(TRUE, FALSE)), size = 2,
      unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      list_combine(list(), indices = list(), size = 2, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Locations 1 and 2 are unmatched.

# list_combine() `unmatched = 'error'` errors pluralize correctly

    Code
      list_combine(list(1, 3), indices = list(1, 3), size = 3, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      list_combine(list(1, 3), indices = list(1, 3), size = 4, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Locations 2 and 4 are unmatched.

---

    Code
      list_combine(list(1, 3), indices = list(1, 3), size = 100, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Locations 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ..., 99, and 100 are unmatched.

# list_combine() `unmatched = 'error'` can't be set when `default` is also set

    Code
      list_combine(list(1), indices = list(1), default = 1, size = 1, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Can't set `default` when `unmatched = "error"`.

# list_combine() `unmatched` is validated

    Code
      list_combine(list(1), indices = list(1), size = 1, unmatched = "e")
    Condition
      Error in `list_combine()`:
      ! `unmatched` must be either "default" or "error".

# list_combine() `multiple` is validated

    Code
      list_combine(list(1), indices = list(1), size = 1, multiple = "a")
    Condition
      Error in `list_combine()`:
      ! `multiple` must be either "last" or "first".

# `NA` indices are considered unmatched locations

    Code
      list_combine(x = list(1, 2:3), indices = list(1, c(NA, 3)), size = 3,
      unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      list_combine(x = list(1, 2:3), indices = list(c(TRUE, FALSE, FALSE), c(FALSE,
        NA, TRUE)), size = 3, unmatched = "error")
    Condition
      Error in `list_combine()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

# `x` must be a list

    Code
      list_combine(1, indices = list(1), size = 1)
    Condition
      Error in `list_combine()`:
      ! `x` must be a list, not the number 1.

---

    Code
      list_combine(1, indices = list(1), size = 1, error_call = call("foo"), x_arg = "arg")
    Condition
      Error in `foo()`:
      ! `arg` must be a list, not the number 1.

---

    Code
      list_combine(data.frame(x = 1), indices = list(1), size = 1)
    Condition
      Error in `list_combine()`:
      ! `x` must be a list, not a <data.frame> object.

# `indices` must be a list

    Code
      list_combine(list(1), indices = 1, size = 1)
    Condition
      Error in `list_combine()`:
      ! `indices` must be a list, not the number 1.

---

    Code
      list_combine(list(1), indices = 1, size = 1, error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! `indices` must be a list, not the number 1.

---

    Code
      list_combine(list(1), indices = data.frame(x = 1), size = 1)
    Condition
      Error in `list_combine()`:
      ! `indices` must be a list, not a <data.frame> object.

# `x` and `indices` must be lists of the same size

    Code
      list_combine(list(1, 2), indices = list(1), size = 1)
    Condition
      Error in `list_combine()`:
      ! `indices` must have size 2, not size 1.

# NULL is a valid index

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements past the end.
      i Location 2 doesn't exist.
      i There is only 1 element.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements past the end.
      i Location 2 doesn't exist.
      i There is only 1 element.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements past the end.
      i Location 2 doesn't exist.
      i There is only 1 element.

# combining recycles elements of x to the size of the index

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `arg[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

# combining takes the common type

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <double> and `x[[2]]` <character>.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <vctrs_foobar> and `x[[2]]` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <vctrs_foobar> and `x[[2]]` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# common type failure uses positional errors

    Code
      x <- list(1, a = "x", 2)
      (expect_error(list_combine(x, indices = list(2, 1, 3), size = 3)))
    Output
      <error/vctrs_error_ptype2>
      Error in `list_combine()`:
      ! Can't combine `x[[1]]` <double> and `x$a` <character>.
    Code
      (expect_error(list_combine(x, indices = list(2, 1, 3), size = 3, ptype = double()))
      )
    Output
      <error/vctrs_error_cast>
      Error in `list_combine()`:
      ! Can't convert `x$a` <character> to <double>.
    Code
      y <- list(1, a = 2.5)
      (expect_error(list_combine(y, indices = list(2, 1), size = 2, ptype = integer()))
      )
    Output
      <error/vctrs_error_cast_lossy>
      Error in `list_combine()`:
      ! Can't convert from `x$a` <double> to <integer> due to loss of precision.
      * Locations: 1

# can specify a ptype to override common type

    Code
      list_combine(x, indices = indices, size = 2, ptype = integer())
    Condition <vctrs_error_cast_lossy>
      Error in `list_combine()`:
      ! Can't convert from `x[[1]]` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      list_combine(x, indices = indices, size = 2, ptype = integer(), error_call = call(
        "foo"), x_arg = "arg")
    Condition <vctrs_error_cast_lossy>
      Error in `foo()`:
      ! Can't convert from `arg[[1]]` <double> to <integer> due to loss of precision.
      * Locations: 1

# outer names are recycled in the right order

    Code
      list_combine(x, indices = list(c(1, 2), 3), size = 3)
    Condition
      Error in `list_combine()`:
      ! Can't merge the outer name `x` with a vector of length > 1.
      Please supply a `.name_spec` specification.

# list_combine() can repair names quietly

    Code
      res <- list_combine(vec_chop(x, indices = indices), indices = indices, size = 3,
      name_repair = "unique_quiet")

---

    Code
      res <- list_combine(vec_chop(x, indices = indices), indices = indices, size = 3,
      name_repair = "universal_quiet")

# list_combine() errors on unsupported location values

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain `0` values.
      i It has a `0` value at location 1.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain `0` values.
      i It has a `0` value at location 1.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain `0` values.
      i It has a `0` value at location 1.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain negative locations.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain negative locations.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript can't contain negative locations.

# list_combine() fallback doesn't support `name_spec` or `ptype`

    Code
      foo <- structure(foobar(1), foo = "foo")
      bar <- structure(foobar(2), bar = "bar")
      (expect_error(with_c_foobar(list_combine(list(foo, bar), indices = list(1, 2),
      size = 2, name_spec = "{outer}_{inner}")), "name specification"))
    Output
      <error/rlang_error>
      Error in `list_combine()`:
      ! Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.
    Code
      (expect_error(with_c_foobar(list_combine(list(foo, bar), indices = list(1, 2),
      size = 2, name_spec = "{outer}_{inner}", error_call = call("foo"))),
      "name specification"))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.
    Code
      x <- list(foobar(1))
      (expect_error(with_c_foobar(list_combine(x, indices = list(1), size = 1, ptype = "")),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error in `list_combine()`:
      ! Can't convert `x[[1]]` <vctrs_foobar> to <character>.

# list_combine() does not support non-numeric S3 indices

    Code
      (expect_error(list_combine(list(1), indices = list(factor("x")), size = 1),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements.
      x Subscript must be numeric, not the string "x".
    Code
      (expect_error(list_combine(list(1), indices = list(foobar(1L)), size = 1),
      class = "vctrs_error_subscript_type"))
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements.
      x Subscript must be numeric, not a <vctrs_foobar> object.

# `list_combine()` with logical `indices` checks `indices` size

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be numeric, not a logical vector.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be numeric, not a logical vector.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error:
      ! Can't subset elements.
      x Subscript must be numeric, not a logical vector.

# `multiple` shows correctly indexed errors

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 3.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[2]]` (size 3) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x, indices = indices, size = size, default = default, unmatched = unmatched,
        multiple = multiple, slice_x = slice_x, ptype = ptype, name_spec = name_spec,
        name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar, indices = indices, size = size, default = default_foobar,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

---

    Code
      list_combine(x_foobar_c, indices = indices, size = size, default = default_foobar_c,
        unmatched = unmatched, multiple = multiple, slice_x = slice_x, ptype = ptype,
        name_spec = name_spec, name_repair = name_repair, x_arg = x_arg, indices_arg = indices_arg,
        default_arg = default_arg)
    Condition
      Error in `list_combine()`:
      ! Can't recycle `x[[1]]` (size 2) to size 4.

