# `x` must be a list

    Code
      list_unchop(1, list(1))
    Condition
      Error in `list_unchop()`:
      ! `x` must be a list, not a number.

---

    Code
      list_unchop(1, list(1), error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! `x` must be a list, not a number.

---

    Code
      list_unchop(data.frame(x = 1), list(1))
    Condition
      Error in `list_unchop()`:
      ! `x` must be a list, not a <data.frame> object.

# `indices` must be a list

    Code
      list_unchop(list(1), 1)
    Condition
      Error in `list_unchop()`:
      ! `indices` must be a list, not a number.

---

    Code
      list_unchop(list(1), 1, error_call = call("foo"))
    Condition
      Error in `foo()`:
      ! `indices` must be a list, not a number.

---

    Code
      list_unchop(list(1), data.frame(x = 1))
    Condition
      Error in `list_unchop()`:
      ! `indices` must be a list, not a <data.frame> object.

# unchopping recycles elements of x to the size of the index

    Code
      (expect_error(list_unchop(x, indices = indices)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `list_unchop()`:
      ! Can't recycle `..1` (size 2) to size 3.
    Code
      (expect_error(list_unchop(x, indices = indices, error_call = call("foo"))))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `foo()`:
      ! Can't recycle `..1` (size 2) to size 3.

# unchopping takes the common type

    Code
      (expect_error(list_unchop(x, indices), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't combine `..1` <double> and `..2` <character>.
    Code
      (expect_error(list_unchop(x, indices, error_call = call("foo")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `foo()`:
      ! Can't combine `..1` <double> and `..2` <character>.

# can specify a ptype to override common type

    Code
      (expect_error(list_unchop(x, indices = indices, ptype = integer())))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `list_unchop()`:
      ! Can't convert from `..1` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(list_unchop(x, indices = indices, ptype = integer(), error_call = call(
        "foo"))))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `foo()`:
      ! Can't convert from `..1` <double> to <integer> due to loss of precision.
      * Locations: 1

# list_unchop() errors on unsupported location values

    Code
      (expect_error(list_unchop(list(1, 2), list(c(1, 2), 0)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      (expect_error(list_unchop(list(1), list(-1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain negative locations.

# list_unchop() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(list_unchop(list(x, y)), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
    Code
      (expect_error(list_unchop(list(x, y), error_call = call("foo")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `foo()`:
      ! Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# list_unchop() fails with complex foreign S4 classes

    Code
      joe <- .Counts(c(1L, 2L), name = "Joe")
      jane <- .Counts(3L, name = "Jane")
      (expect_error(list_unchop(list(joe, jane)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
    Code
      (expect_error(list_unchop(list(joe, jane), error_call = call("foo")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `foo()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# list_unchop() falls back for S4 classes with a registered c() method

    Code
      (expect_error(list_unchop(list(joe, 1, jane), list(c(1, 2), 3, 4)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <double>.

# list_unchop() fallback doesn't support `name_spec` or `ptype`

    Code
      foo <- structure(foobar(1), foo = "foo")
      bar <- structure(foobar(2), bar = "bar")
      (expect_error(with_c_foobar(list_unchop(list(foo, bar), name_spec = "{outer}_{inner}")),
      "name specification"))
    Output
      <error/rlang_error>
      Error in `list_unchop()`:
      ! Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.
    Code
      (expect_error(with_c_foobar(list_unchop(list(foo, bar), name_spec = "{outer}_{inner}",
      error_call = call("foo"))), "name specification"))
    Output
      <error/rlang_error>
      Error in `foo()`:
      ! Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.
    Code
      (expect_error(with_c_foobar(list_unchop(list(foobar(1)), ptype = "")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't convert <vctrs_foobar> to <character>.

# list_unchop() does not support non-numeric S3 indices

    Code
      (expect_error(list_unchop(list(1), list(factor("x"))), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `character`.
      i It must be numeric.
    Code
      (expect_error(list_unchop(list(1), list(foobar(1L))), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `vctrs_foobar`.
      i It must be numeric.

# can ignore names in `list_unchop()` by providing a `zap()` name-spec (#232)

    Code
      (expect_error(list_unchop(list(a = c(b = 1:2)))))
    Output
      <error/rlang_error>
      Error in `list_unchop()`:
      ! Can't merge the outer name `a` with a vector of length > 1.
      Please supply a `.name_spec` specification.
    Code
      (expect_error(list_unchop(list(a = c(b = 1:2)), error_call = call("foo"))))
    Output
      <error/rlang_error>
      Error in `list_unchop()`:
      ! Can't merge the outer name `a` with a vector of length > 1.
      Please supply a `.name_spec` specification.

---

    Code
      (expect_error(list_unchop(list(a = c(b = letters), b = 3L), name_spec = zap()),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't combine `a` <character> and `b` <integer>.
    Code
      (expect_error(list_unchop(list(a = c(foo = 1:2), b = c(bar = "")), indices = list(
        2:1, 3), name_spec = zap()), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `list_unchop()`:
      ! Can't combine `a` <integer> and `b` <character>.

