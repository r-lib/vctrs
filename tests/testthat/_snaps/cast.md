# Casting to named argument mentions 'match type <foo>'

    Code
      vec_cast(1, "", x_arg = "foo", to_arg = "bar")
    Condition
      Error:
      ! Can't convert `foo` <double> to match type of `bar` <character>.

---

    Code
      vec_cast(1, "", x_arg = "foo")
    Condition
      Error:
      ! Can't convert `foo` <double> to <character>.

# cast errors create helpful messages (#57, #225)

    Code
      vec_cast(1.5, 10L)
    Condition
      Error:
      ! Can't convert from `1.5` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_cast(factor("foo"), 10)
    Condition
      Error:
      ! Can't convert `factor("foo")` <factor<c1562>> to <double>.

---

    Code
      x <- tibble(a = tibble(b = 1.5))
      y <- tibble(a = tibble(b = 10L))
      vec_cast(x, y)
    Condition
      Error:
      ! Can't convert from `x$a$b` <double> to `a$b` <integer> due to loss of precision.
      * Locations: 1

---

    Code
      x <- tibble(a = tibble(b = factor("foo")))
      y <- tibble(a = tibble(b = 10))
      vec_cast(x, y)
    Condition
      Error:
      ! Can't convert `x$a$b` <factor<c1562>> to match type of `a$b` <double>.

---

    Code
      x <- tibble(a = tibble(b = factor("foo")))
      y <- tibble(a = tibble(b = 10))
      vec_cast_common(x, y)
    Condition
      Error:
      ! Can't combine `..1$a$b` <factor<c1562>> and `..2$a$b` <double>.

# vec_cast() only attempts to fall back if `to` is a data frame (#1568)

    Code
      (expect_error(vec_cast(foobar(mtcars), 1), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert `foobar(mtcars)` <vctrs_foobar> to <double>.

# can signal deprecation warnings for lossy casts

    Code
      (expect_warning(expect_true(lossy_cast())))
    Output
      <warning/lifecycle_warning_deprecated>
      Warning:
      Coercion with lossy casts was deprecated in vctrs 0.2.0.
      i Please use `allow_lossy_cast()` instead.
      i We detected a lossy transformation from `x` <fct> to `to` <fct>. The result will contain lower-resolution values or missing values. To suppress this warning, wrap your code with `allow_lossy_cast()`.

# can cast to unspecified `NA` with `vec_cast()` and `vec_cast_common()` (#2099)

    Code
      vec_cast(TRUE, to = unspecified(1))
    Condition
      Error:
      ! Can't convert `TRUE` <logical> to <vctrs_unspecified>.

