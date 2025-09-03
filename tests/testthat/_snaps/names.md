# vec_as_names() validates `repair`

    Code
      (expect_error(my_vec_as_names("x", my_repair = "foo"), "can't be \"foo\""))
    Output
      <error/rlang_error>
      Error in `my_vec_as_names()`:
      ! `my_repair` can't be "foo". See `?vctrs::vec_as_names`.
    Code
      (expect_error(my_vec_as_names(1, my_repair = 1), "string or a function"))
    Output
      <error/rlang_error>
      Error in `my_vec_as_names()`:
      ! `my_repair` must be a string or a function. See `?vctrs::vec_as_names`.

# vec_as_names() checks unique names

    Code
      (expect_error(my_vec_as_names(chr(NA), my_repair = "check_unique")))
    Output
      <error/rlang_error>
      Error:
      ! Names repair functions can't return `NA` values.
    Code
      (expect_error(my_vec_as_names(chr(""), my_repair = "check_unique")))
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Error in `my_vec_as_names()`:
      ! Names can't be empty.
      x Empty name found at location 1.
    Code
      (expect_error(my_vec_as_names(chr("a", "a"), my_repair = "check_unique")))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `my_vec_as_names()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
      i Use argument `my_repair` to specify repair strategy.
    Code
      (expect_error(my_vec_as_names(chr("..1"), my_repair = "check_unique")))
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error in `my_vec_as_names()`:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..1" at location 1.
    Code
      (expect_error(my_vec_as_names(chr("..."), my_repair = "check_unique")))
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error in `my_vec_as_names()`:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..." at location 1.

# vec_as_names() accepts and checks repair function

    Code
      my_vec_as_names(c("", ""), my_repair = function(nms) "foo")
    Condition
      Error in `my_vec_as_names()`:
      ! Repaired names have length 1 instead of length 2.

# vec_as_names() is noisy by default

    Code
      vec_as_names(c("x", "x"), repair = "unique")
    Message
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
      [1] "x...1" "x...2"
    Code
      vec_as_names(c("x", "x"), repair = "unique", quiet = TRUE)
    Output
      [1] "x...1" "x...2"
    Code
      (expect_error(my_vec_as_names(c("x", "x"), my_repair = "check_unique")))
    Output
      <error/vctrs_error_names_must_be_unique>
      Error in `my_vec_as_names()`:
      ! Names must be unique.
      x These names are duplicated:
        * "x" at locations 1 and 2.
      i Use argument `my_repair` to specify repair strategy.
    Code
      vec_as_names(c("1", "1"), repair = "unique_quiet")
    Output
      [1] "1...1" "1...2"
    Code
      vec_as_names(c("1", "1"), repair = "universal_quiet")
    Output
      [1] "...1...1" "...1...2"
    Code
      vec_as_names(c("1", "1"), repair = "unique_quiet", quiet = TRUE)
    Output
      [1] "1...1" "1...2"
    Code
      vec_as_names(c("1", "1"), repair = "universal_quiet", quiet = TRUE)
    Output
      [1] "...1...1" "...1...2"
    Code
      vec_as_names(c("1", "1"), repair = "unique_quiet", quiet = FALSE)
    Output
      [1] "1...1" "1...2"
    Code
      vec_as_names(c("1", "1"), repair = "universal_quiet", quiet = FALSE)
    Output
      [1] "...1...1" "...1...2"

# validate_minimal_names() checks names

    Code
      (expect_error(validate_minimal_names(1), "must return a character vector"))
    Output
      <error/rlang_error>
      Error:
      ! Names repair functions must return a character vector.
    Code
      (expect_error(validate_minimal_names(NULL), "can't return `NULL`"))
    Output
      <error/rlang_error>
      Error:
      ! Names repair functions can't return `NULL`.
    Code
      (expect_error(validate_minimal_names(chr(NA)), "can't return `NA` values"))
    Output
      <error/rlang_error>
      Error:
      ! Names repair functions can't return `NA` values.

# validate_unique() checks unique names

    Code
      (expect_error(validate_unique(chr(NA)), "`NA`"))
    Output
      <error/rlang_error>
      Error:
      ! Names repair functions can't return `NA` values.
    Code
      (expect_error(validate_unique(chr("")), class = "vctrs_error_names_cannot_be_empty")
      )
    Output
      <error/vctrs_error_names_cannot_be_empty>
      Error:
      ! Names can't be empty.
      x Empty name found at location 1.
    Code
      (expect_error(validate_unique(chr("a", "a")), class = "vctrs_error_names_must_be_unique")
      )
    Output
      <error/vctrs_error_names_must_be_unique>
      Error:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
    Code
      (expect_error(validate_unique(chr("..1")), class = "vctrs_error_names_cannot_be_dot_dot")
      )
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..1" at location 1.
    Code
      (expect_error(validate_unique(chr("...")), class = "vctrs_error_names_cannot_be_dot_dot")
      )
    Output
      <error/vctrs_error_names_cannot_be_dot_dot>
      Error:
      ! Names can't be of the form `...` or `..j`.
      x These names are invalid:
        * "..." at location 1.

# vec_set_names() errors with bad `names`

    Code
      (expect_error(vec_set_names(1, 1), "character vector, not a double"))
    Output
      <error/rlang_error>
      Error in `vec_set_names()`:
      ! `names` must be a character vector, not a double.
    Code
      (expect_error(vec_set_names(1, c("x", "y")), "The size of `names`, 2"))
    Output
      <error/rlang_error>
      Error in `vec_set_names()`:
      ! The size of `names`, 2, must be the same as the size of `x`, 1.

# unique_names() and as_unique_names() are verbose or silent

    Code
      unique_names(1:2)
    Message
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
      [1] "...1" "...2"

---

    Code
      as_unique_names(c("", ""))
    Message
      New names:
      * `` -> `...1`
      * `` -> `...2`
    Output
      [1] "...1" "...2"

# message

    Code
      as_universal_names(c("a b", "b c"))
    Message
      New names:
      * `a b` -> `a.b`
      * `b c` -> `b.c`
    Output
      [1] "a.b" "b.c"

# messages by default

    Code
      vec_repair_names(set_names(1, "a:b"), "universal")
    Message
      New names:
      * `a:b` -> `a.b`
    Output
      a.b 
        1 

---

    Code
      vec_repair_names(set_names(1, "a:b"), ~ make.names(.))
    Message
      New names:
      * `a:b` -> `a.b`
    Output
      a.b 
        1 

# NULL name specs works with scalars

    Code
      (expect_error(vec_c(foo = c(a = 1, b = 2)), "vector of length > 1"))
    Output
      <error/rlang_error>
      Error in `vec_c()`:
      ! Can't merge the outer name `foo` with a vector of length > 1.
      Please supply a `.name_spec` specification.
    Code
      (expect_error(vec_c(foo = 1:2), "vector of length > 1"))
    Output
      <error/rlang_error>
      Error in `vec_c()`:
      ! Can't merge the outer name `foo` with a vector of length > 1.
      Please supply a `.name_spec` specification.
    Code
      (expect_error(vec_c(x = c(xx = 1)), "named vector"))
    Output
      <error/rlang_error>
      Error in `vec_c()`:
      ! Can't merge the outer name `x` with a named vector.
      Please supply a `.name_spec` specification.

# apply_name_spec() checks recyclability of output

    Code
      apply_name_spec(function(...) c("a", "b", "c"), "outer", "inner", n = 2L)
    Condition
      Error:
      ! Can't recycle input of size 3 to size 2.

# vec_as_names() uses internal error if `repair_arg` is not supplied

    Code
      (expect_error(vec_as_names("", repair = "foobar", call = quote(tilt()))))
    Output
      <error/rlang_error>
      Error in `vec_as_names()`:
      ! `repair` can't be "foobar". See `?vctrs::vec_as_names`.
    Code
      (expect_error(vec_as_names("", repair = env(), call = quote(tilt()))))
    Output
      <error/rlang_error>
      Error in `vec_as_names()`:
      ! `repair` must be a string or a function. See `?vctrs::vec_as_names`.

