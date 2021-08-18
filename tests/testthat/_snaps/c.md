# vec_c() includes index in argument tag

    Code
      vec_c(df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `..2$x$y$z` <character>.

---

    Code
      vec_c(df1, df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$x$y$z` <double> and `..3$x$y$z` <character>.

---

    Code
      vec_c(foo = df1, bar = df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `foo$x$y$z` <double> and `bar$x$y$z` <character>.

# vec_c() fails with complex foreign S3 classes

    Code
      x <- structure(foobar(1), attr_foo = "foo")
      y <- structure(foobar(2), attr_bar = "bar")
      (expect_error(vec_c(x, y), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Can't combine `..1` <vctrs_foobar> and `..2` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_c() fails with complex foreign S4 classes

    Code
      joe <- .Counts(c(1L, 2L), name = "Joe")
      jane <- .Counts(3L, name = "Jane")
      (expect_error(vec_c(joe, jane), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Can't combine `..1` <vctrs_Counts> and `..2` <vctrs_Counts>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# vec_c() fallback doesn't support `name_spec` or `ptype`

    Code
      (expect_error(with_c_foobar(vec_c(foobar(1), foobar(2), .name_spec = "{outer}_{inner}")),
      "name specification"))
    Output
      <simpleError: Can't use a name specification with non-vctrs types.
      vctrs methods must be implemented for class `vctrs_foobar`.
      See <https://vctrs.r-lib.org/articles/s3-vector.html>.>
    Code
      (expect_error(with_c_foobar(vec_c(foobar(1), foobar(2), .ptype = "")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Can't convert <vctrs_foobar> to <character>.

# can ignore names in `vec_c()` by providing a `zap()` name-spec (#232)

    Code
      (expect_error(vec_c(a = c(b = letters), b = 1, .name_spec = zap()), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Can't combine `a` <character> and `b` <double>.

