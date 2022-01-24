# vec_unchop() errors on unsupported location values

    Code
      (expect_error(vec_unchop(list(1, 2), list(c(1, 2), 0)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain `0` values.
      i It has a `0` value at location 1.
    Code
      (expect_error(vec_unchop(list(1), list(-1)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript can't contain negative locations.

# vec_unchop() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(vec_unchop(list(x, y)), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_ptype2()`:
      ! Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_unchop() fails with complex foreign S4 classes

    Code
      joe <- .Counts(c(1L, 2L), name = "Joe")
      jane <- .Counts(3L, name = "Jane")
      (expect_error(vec_unchop(list(joe, jane)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_ptype2()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_unchop() falls back for S4 classes with a registered c() method

    Code
      (expect_error(vec_unchop(list(joe, 1, jane), list(c(1, 2), 3, 4)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_ptype2()`:
      ! Can't combine `..1` <vctrs_Counts> and `..2` <double>.

# vec_unchop() fallback doesn't support `name_spec` or `ptype`

    Code
      foo <- structure(foobar(1), foo = "foo")
      bar <- structure(foobar(2), bar = "bar")
      (expect_error(with_c_foobar(vec_unchop(list(foo, bar), name_spec = "{outer}_{inner}")),
      "name specification"))
    Output
      <simpleError: Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.>
    Code
      (expect_error(with_c_foobar(vec_unchop(list(foobar(1)), ptype = "")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't convert <vctrs_foobar> to <character>.

# vec_unchop() does not support non-numeric S3 indices

    Code
      (expect_error(vec_unchop(list(1), list(factor("x"))), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `character`.
      i It must be numeric.
    Code
      (expect_error(vec_unchop(list(1), list(foobar(1L))), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Must subset elements with a valid subscript vector.
      x Subscript has the wrong type `vctrs_foobar`.
      i It must be numeric.

# can ignore names in `vec_unchop()` by providing a `zap()` name-spec (#232)

    Code
      (expect_error(vec_unchop(list(a = c(b = letters), b = 3L), name_spec = zap()),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_ptype2()`:
      ! Can't combine `a` <character> and `b` <integer>.
    Code
      (expect_error(vec_unchop(list(a = c(foo = 1:2), b = c(bar = "")), indices = list(
        2:1, 3), name_spec = zap()), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_default_ptype2()`:
      ! Can't combine `a` <integer> and `b` <character>.

