# base coercions are symmetric and unchanging

    Code
      mat
    Output
                logical   integer   double   character   raw   list  
      logical   "logical" "integer" "double" NA          NA    NA    
      integer   "integer" "integer" "double" NA          NA    NA    
      double    "double"  "double"  "double" NA          NA    NA    
      character NA        NA        NA       "character" NA    NA    
      raw       NA        NA        NA       NA          "raw" NA    
      list      NA        NA        NA       NA          NA    "list"

# vec_ptype2() data frame methods builds argument tags

    Code
      vec_ptype2("foo", 10)
    Condition
      Error:
      ! Can't combine `"foo"` <character> and `10` <double>.

---

    Code
      df1 <- tibble(x = tibble(y = tibble(z = 1)))
      df2 <- tibble(x = tibble(y = tibble(z = "a")))
      vec_ptype2(df1, df2)
    Condition
      Error:
      ! Can't combine `df1$x$y$z` <double> and `df2$x$y$z` <character>.

# can override scalar vector error message for base scalar types

    Code
      (expect_error(vec_ptype2(NULL, quote(x), y_arg = "foo"), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      Error:
      ! `foo` must be a vector, not a symbol.
    Code
      (expect_error(vec_ptype2(quote(x), NULL, x_arg = "foo"), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      Error:
      ! `foo` must be a vector, not a symbol.

# can override scalar vector error message for S3 types

    Code
      (expect_error(vec_ptype2(NULL, foobar(), y_arg = "foo"), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      Error:
      ! `foo` must be a vector, not a <vctrs_foobar> object.
    Code
      (expect_error(vec_ptype2(foobar(), NULL, x_arg = "foo"), class = "vctrs_error_scalar_type")
      )
    Output
      <error/vctrs_error_scalar_type>
      Error:
      ! `foo` must be a vector, not a <vctrs_foobar> object.

# ptype2 and cast errors when same class fallback is impossible are informative

    Code
      (expect_error(vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert `foobar(1, bar = TRUE)` <vctrs_foobar> to <vctrs_foobar>.
    Code
      (expect_error(vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error:
      ! Can't combine `foobar(1, bar = TRUE)` <vctrs_foobar> and `foobar(2, baz = TRUE)` <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# Incompatible attributes bullets are not show when methods are implemented

    Code
      with_foobar_cast <- (function(expr) {
        with_methods(vec_cast.vctrs_foobar = function(...) NULL,
        vec_cast.vctrs_foobar.vctrs_foobar = function(x, to, ...) vec_default_cast(x,
          to, ...), expr)
      })
      with_foobar_ptype2 <- (function(expr) {
        with_methods(vec_ptype2.vctrs_foobar = function(...) NULL,
        vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) vec_default_ptype2(
          x, y, ...), expr)
      })
      (expect_error(with_foobar_cast(vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE))),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert `foobar(1, bar = TRUE)` <vctrs_foobar> to <vctrs_foobar>.
    Code
      (expect_error(with_foobar_ptype2(vec_ptype2(foobar(1, bar = TRUE), foobar(2,
        baz = TRUE))), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error:
      ! Can't combine `foobar(1, bar = TRUE)` <vctrs_foobar> and `foobar(2, baz = TRUE)` <vctrs_foobar>.

