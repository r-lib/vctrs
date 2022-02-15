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

