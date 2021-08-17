# cast errors create helpful messages (#57, #225)

    Code
      vec_cast(1.5, 10L)
    Error <vctrs_error_cast_lossy>
      Can't convert from <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_cast(factor("foo"), 10)
    Error <vctrs_error_incompatible_type>
      Can't convert <factor<c1562>> to <double>.

---

    Code
      x <- tibble(a = tibble(b = 1.5))
      y <- tibble(a = tibble(b = 10L))
      vec_cast(x, y)
    Error <vctrs_error_cast_lossy>
      Can't convert from `a$b` <double> to `a$b` <integer> due to loss of precision.
      * Locations: 1

---

    Code
      x <- tibble(a = tibble(b = factor("foo")))
      y <- tibble(a = tibble(b = 10))
      vec_cast(x, y)
    Error <vctrs_error_incompatible_type>
      Can't convert `a$b` <factor<c1562>> to match type of `a$b` <double>.

---

    Code
      x <- tibble(a = tibble(b = factor("foo")))
      y <- tibble(a = tibble(b = 10))
      vec_cast_common(x, y)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$a$b` <factor<c1562>> and `..2$a$b` <double>.

