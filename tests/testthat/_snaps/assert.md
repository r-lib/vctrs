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

