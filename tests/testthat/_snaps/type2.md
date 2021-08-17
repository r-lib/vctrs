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
    Error <vctrs_error_incompatible_type>
      Can't combine <character> and <double>.

---

    Code
      df1 <- tibble(x = tibble(y = tibble(z = 1)))
      df2 <- tibble(x = tibble(y = tibble(z = "a")))
      vec_ptype2(df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `x$y$z` <double> and `x$y$z` <character>.

# can override scalar vector error message for base scalar types

    Code
      vec_ptype2(NULL, quote(x), y_arg = "foo")
    Error <vctrs_error_scalar_type>
      `foo` must be a vector, not a symbol.

---

    Code
      vec_ptype2(quote(x), NULL, x_arg = "foo")
    Error <vctrs_error_scalar_type>
      `foo` must be a vector, not a symbol.

# can override scalar vector error message for S3 types

    Code
      vec_ptype2(NULL, foobar(), y_arg = "foo")
    Error <vctrs_error_scalar_type>
      `foo` must be a vector, not a <vctrs_foobar> object.

---

    Code
      vec_ptype2(foobar(), NULL, x_arg = "foo")
    Error <vctrs_error_scalar_type>
      `foo` must be a vector, not a <vctrs_foobar> object.

# ptype2 and cast errors when same class fallback is impossible are informative

    Code
      vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE))
    Error <vctrs_error_incompatible_type>
      Can't convert <vctrs_foobar> to <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

---

    Code
      vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE))
    Error <vctrs_error_incompatible_type>
      Can't combine <vctrs_foobar> and <vctrs_foobar>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.

# common type errors don't mention columns if they are compatible

    Code
      vec_cast_no_fallback(foo, bar)
    Error <vctrs_error_incompatible_type>
      Can't convert <vctrs_foo> to <vctrs_bar>.

# common type warnings for data frames take attributes into account

    Code
      vec_ptype2_fallback(foobar_bud, foobar_boo)
    Warning <warning>
      Can't combine <vctrs_foobar> and <vctrs_foobar>; falling back to <data.frame>.
      x Some attributes are incompatible.
      i The author of the class should implement vctrs methods.
      i See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>.
    Output
       [1] mpg  cyl  disp hp   drat wt   qsec vs   am   gear carb
      <0 rows> (or 0-length row.names)

# Incompatible attributes bullets are not show when methods are implemented

    Code
      with_foobar_cast(vec_cast(foobar(1, bar = TRUE), foobar(2, baz = TRUE)))
    Error <vctrs_error_incompatible_type>
      Can't convert <vctrs_foobar> to <vctrs_foobar>.

---

    Code
      with_foobar_ptype2(vec_ptype2(foobar(1, bar = TRUE), foobar(2, baz = TRUE)))
    Error <vctrs_error_incompatible_type>
      Can't combine <vctrs_foobar> and <vctrs_foobar>.

# For reference, warning for incompatible classes

    Code
      vec_ptype2_fallback(foobar(mtcars), foobaz(mtcars))
    Warning <warning>
      Can't combine <vctrs_foobar> and <vctrs_foobaz>; falling back to <data.frame>.
    Output
       [1] mpg  cyl  disp hp   drat wt   qsec vs   am   gear carb
      <0 rows> (or 0-length row.names)

# For reference, error when fallback is disabled

    Code
      vec_ptype2_no_fallback(foobar(mtcars), foobaz(mtcars))
    Error <vctrs_error_incompatible_type>
      Can't combine <vctrs_foobar> and <vctrs_foobaz>.

