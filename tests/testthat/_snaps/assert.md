# obj_check_vector() errors on scalars

    Code
      obj_check_vector(quote(foo))
    Condition
      Error:
      ! `quote(foo)` must be a vector, not a symbol.

---

    Code
      obj_check_vector(foobar())
    Condition
      Error:
      ! `foobar()` must be a vector, not a <vctrs_foobar> object.

# obj_check_vector() error respects `arg` and `call`

    Code
      my_check_vector(foobar())
    Condition
      Error in `my_check_vector()`:
      ! `foo` must be a vector, not a <vctrs_foobar> object.

# assertion failures are explained

    Code
      vec_assert(lgl(), chr())
    Condition
      Error:
      ! `lgl()` must be a vector with type <character>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), factor())
    Condition
      Error:
      ! `lgl()` must be a vector with type <factor<>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), factor(levels = "foo"))
    Condition
      Error:
      ! `lgl()` must be a vector with type <factor<c1562>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(factor(levels = "bar"), factor(levels = "foo"))
    Condition
      Error:
      ! `factor(levels = "bar")` must be a vector with type <factor<c1562>>.
      Instead, it has type <factor<9f154>>.

---

    Code
      vec_assert(factor(), chr())
    Condition
      Error:
      ! `factor()` must be a vector with type <character>.
      Instead, it has type <factor<>>.

---

    Code
      vec_assert(lgl(), data.frame())
    Condition
      Error:
      ! `lgl()` must be a vector with type <data.frame<>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), data.frame(x = 1))
    Condition
      Error:
      ! `lgl()` must be a vector with type <data.frame<x:double>>.
      Instead, it has type <logical>.

---

    Code
      vec_assert(lgl(), data.frame(x = 1, y = 2))
    Condition
      Error:
      ! `lgl()` must be a vector with type:
      
        <data.frame<
          x: double
          y: double
        >>
      
      Instead, it has type <logical>.

---

    Code
      vec_assert(data.frame(), chr())
    Condition
      Error:
      ! `data.frame()` must be a vector with type <character>.
      Instead, it has type <data.frame<>>.

---

    Code
      vec_assert(data.frame(x = 1), chr())
    Condition
      Error:
      ! `data.frame(x = 1)` must be a vector with type <character>.
      Instead, it has type <data.frame<x:double>>.

---

    Code
      vec_assert(data.frame(x = 1), data.frame(x = "foo"))
    Condition
      Error:
      ! `data.frame(x = 1)` must be a vector with type <data.frame<x:character>>.
      Instead, it has type <data.frame<x:double>>.

---

    Code
      vec_assert(data.frame(x = 1), data.frame(x = "foo", y = 2))
    Condition
      Error:
      ! `data.frame(x = 1)` must be a vector with type:
      
        <data.frame<
          x: character
          y: double
        >>
      
      Instead, it has type <data.frame<x:double>>.

---

    Code
      vec_assert(data.frame(x = 1, y = 2), chr())
    Condition
      Error:
      ! `data.frame(x = 1, y = 2)` must be a vector with type <character>.
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>

---

    Code
      vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo"))
    Condition
      Error:
      ! `data.frame(x = 1, y = 2)` must be a vector with type <data.frame<x:character>>.
      Instead, it has type:
      
        <data.frame<
          x: double
          y: double
        >>

---

    Code
      vec_assert(data.frame(x = 1, y = 2), data.frame(x = "foo", y = 2))
    Condition
      Error:
      ! `data.frame(x = 1, y = 2)` must be a vector with type:
      
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
      Error in `vec_assert()`:
      ! `size` must be length 1, not length 2.
    Code
      (expect_error(vec_assert(1, size = 1.5)))
    Output
      <error/vctrs_error_cast_lossy>
      Error in `vec_assert()`:
      ! Can't convert from `size` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      (expect_error(vec_assert(1, size = "x")))
    Output
      <error/vctrs_error_cast>
      Error in `vec_assert()`:
      ! Can't convert `size` <character> to <integer>.

# vec_check_size() errors on the wrong size

    Code
      vec_check_size(1:5, size = 1L)
    Condition
      Error:
      ! `1:5` must have size 1, not size 5.

---

    Code
      vec_check_size(1:5, size = 10L)
    Condition
      Error:
      ! `1:5` must have size 10, not size 5.

# vec_check_size() errors on scalars

    Code
      vec_check_size(quote(foo), size = 1L)
    Condition
      Error:
      ! `quote(foo)` must be a vector, not a symbol.

---

    Code
      vec_check_size(foobar(), size = 1L)
    Condition
      Error:
      ! `foobar()` must be a vector, not a <vctrs_foobar> object.

# vec_check_size() error respects `arg` and `call`

    Code
      my_check_size(1L, size = 5L)
    Condition
      Error in `my_check_size()`:
      ! `foo` must have size 5, not size 1.

---

    Code
      my_check_size(foobar(), size = 5L)
    Condition
      Error in `my_check_size()`:
      ! `foo` must be a vector, not a <vctrs_foobar> object.

# vec_check_size() validates `size`

    Code
      vec_check_size(1, size = "x")
    Condition
      Error in `vec_check_size()`:
      ! `size` must be a scalar integer or double.

---

    Code
      vec_check_size(1, size = c(1L, 2L))
    Condition
      Error in `vec_check_size()`:
      ! `size` must be a scalar integer or double.

---

    Code
      vec_check_size(1, size = 1.5)
    Condition
      Error in `vec_check_size()`:
      ! `size` must be a whole number, not a decimal number.

# list_all_vectors() works

    Code
      (expect_error(list_all_vectors(env())))
    Output
      <error/rlang_error>
      Error in `list_all_vectors()`:
      ! `x` must be a list, not an environment.

# obj_check_list() works

    Code
      my_function <- (function(my_arg) obj_check_list(my_arg))
      (expect_error(my_function(env())))
    Output
      <error/rlang_error>
      Error in `my_function()`:
      ! `my_arg` must be a list, not an environment.

# obj_check_list() uses a special error when `arg` is the empty string (#1604)

    Code
      obj_check_list(1, arg = "")
    Condition
      Error:
      ! Input must be a list, not the number 1.

# obj_check_list() and list_check_all_vectors() work

    Code
      my_function <- (function(my_arg) list_check_all_vectors(my_arg))
      (expect_error(my_function(env())))
    Output
      <error/rlang_error>
      Error in `list_check_all_vectors()`:
      ! `x` must be a list, not an environment.
    Code
      (expect_error(my_function(list(1, env()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! `my_arg[[2]]` must be a vector, not an environment.
    Code
      (expect_error(my_function(list(1, name = env()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! `my_arg$name` must be a vector, not an environment.
    Code
      (expect_error(my_function(list(1, foo = env()))))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! `my_arg$foo` must be a vector, not an environment.

# list_check_all_size() works

    Code
      my_function <- (function(my_arg, size) list_check_all_size(my_arg, size))
      (expect_error(list_check_all_size(list(1:2, 1:3), 2)))
    Output
      <error/vctrs_error_assert_size>
      Error:
      ! `list(1:2, 1:3)[[2]]` must have size 2, not size 3.
    Code
      (expect_error(my_function(list(1:2, 1:3), 2)))
    Output
      <error/vctrs_error_assert_size>
      Error in `my_function()`:
      ! `my_arg[[2]]` must have size 2, not size 3.
    Code
      (expect_error(my_function(list(NULL, 1:2), 2)))
    Output
      <error/vctrs_error_assert_size>
      Error in `my_function()`:
      ! `my_arg[[1]]` must have size 2, not size 0.

# list_all_size() and list_check_all_size() error on scalars

    Code
      (expect_error(list_all_size(x, 2)))
    Output
      <error/vctrs_error_scalar_type>
      Error in `list_all_size()`:
      ! `x[[1]]` must be a vector, not an environment.
    Code
      my_function <- (function(my_arg, size) list_check_all_size(my_arg, size))
      (expect_error(my_function(x, 2)))
    Output
      <error/vctrs_error_scalar_type>
      Error in `my_function()`:
      ! `my_arg[[1]]` must be a vector, not an environment.

# list_all_size() and list_check_all_size() throw error using internal call on non-list input

    Code
      (expect_error(list_all_size(1, 2)))
    Output
      <error/rlang_error>
      Error in `list_all_size()`:
      ! `x` must be a list, not the number 1.
    Code
      (expect_error(list_check_all_size(1, 2, arg = "arg", call = call("foo"))))
    Output
      <error/rlang_error>
      Error in `list_check_all_size()`:
      ! `x` must be a list, not the number 1.

# list_all_size() and list_check_all_size() validate `size`

    Code
      (expect_error(list_all_size(list(), size = "x")))
    Output
      <error/rlang_error>
      Error in `list_all_size()`:
      ! `size` must be a scalar integer or double.
    Code
      (expect_error(list_check_all_size(list(), size = "x")))
    Output
      <error/rlang_error>
      Error in `list_check_all_size()`:
      ! `size` must be a scalar integer or double.

# informative messages when 1d array doesn't match vector

    Code
      (expect_error(vec_assert(x, int())))
    Output
      <error/vctrs_error_assert_ptype>
      Error:
      ! `x` must be a vector with type <integer>.
      Instead, it has type <integer[1d]>.

