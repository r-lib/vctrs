# assertion failures are explained

    Code
      vec_assert(lgl(), chr())
    Error <vctrs_error_assert_ptype>
      `lgl()` must be a vector with type <character>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), factor())
    Error <vctrs_error_assert_ptype>
      `lgl()` must be a vector with type <factor<>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), factor(levels = "foo"))
    Error <vctrs_error_assert_ptype>
      `lgl()` must be a vector with type <factor<c1562>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(factor(levels = "bar"), factor(levels = "foo"))
    Error <vctrs_error_assert_ptype>
      `factor(levels = "bar")` must be a vector with type <factor<c1562>>.
      Instead, it has type <factor<9f154>>.

---

    Code
      vec_assert(factor(), chr())
    Error <vctrs_error_assert_ptype>
      `factor()` must be a vector with type <character>.
      Instead, it has type <factor<>>.

---

    Code
      vec_assert(lgl(), data.frame())
    Error <vctrs_error_assert_ptype>
      `lgl()` must be a vector with type <data.frame<>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), data.frame(x = 1))
    Error <vctrs_error_assert_ptype>
      `lgl()` must be a vector with type <data.frame<x:double>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), data.frame(x = 1, y = 2))
    Error <vctrs_error_assert_ptype>
      `lgl()` must be a vector with type:
      
        <data.frame<
          x: double
          y: double
        >>
      
      Instead, it has type <logical>.

---

    Code
      vec_assert(data.frame(), chr())
    Error <vctrs_error_assert_ptype>
      `data.frame()` must be a vector with type <character>.
      Instead, it has type <data.frame<>>.

---

    Code
      vec_assert(data.frame(x = 1), chr())
    Error <vctrs_error_assert_ptype>
      `data.frame(x = 1)` must be a vector with type <character>.
      Instead, it has type <data.frame<x:double>>.

---

    Code
      vec_assert(data.frame(x = 1), data.frame(x = "foo"))
    Error <vctrs_error_assert_ptype>
      `data.frame(x = 1)` must be a vector with type <data.frame<x:character>>.
      Instead, it has type <data.frame<x:double>>.

---

    Code
      vec_assert(data.frame(x = 1), data.frame(x = "foo", y = 2))
    Error <vctrs_error_assert_ptype>
      `data.frame(x = 1)` must be a vector with type:
      
        <data.frame<
          x: character
          y: double
        >>
      
      Instead, it has type <data.frame<x:double>>.

---

    Code
      vec_assert(data.frame(x = 1, y = 2), chr())
    Error <vctrs_error_assert_ptype>
      `data.frame(x = 1, y = 2)` must be a vector with type <character>.
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>

---

    Code
      vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo"))
    Error <vctrs_error_assert_ptype>
      `data.frame(x = 1, y = 2)` must be a vector with type <data.frame<x:character>>.
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>

---

    Code
      vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo", y = 2))
    Error <vctrs_error_assert_ptype>
      `data.frame(x = 1, y = 2)` must be a vector with type:
      
        <data.frame<
          x: character
          y: double
        >>
      
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>

# vec_assert() validates `size` (#1470)

    Code
      (expect_error(vec_assert(1, size = c(2, 3))))
    Output
      <error/rlang_error>
      Error in `vec_assert()`: `size` must be length 1, not length 2.
    Code
      (expect_error(vec_assert(1, size = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `stop_lossy_cast()`: Can't convert from `size` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_assert(1, size = "x")))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `stop_incompatible()`: Can't convert `size` <character> to <integer>.

