# vec_as_short_length() checks inputs

    Code
      (expect_error(my_function(-1)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` must be a positive number or zero.
    Code
      (expect_error(my_function(1:2)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` must be a single number, not an integer vector of length 2.
    Code
      (expect_error(my_function(1.5)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` must be a whole number, not a fractional number.
    Code
      (expect_error(my_function(NA)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` must be a single number, not `NA`.
    Code
      (expect_error(my_function(na_int)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` must be a single number, not an integer `NA`.
    Code
      (expect_error(my_function("foo")))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` must be a single number, not a string.
    Code
      (expect_error(my_function(foobar(1:2))))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `vec_as_short_length()`:
      ! Can't convert `my_arg` <vctrs_foobar> to <double>.
    Code
      (expect_error(my_function(.Machine$integer.max + 1)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` is too large a number and long vectors are not supported.
    Code
      (expect_error(my_function(.Machine$double.xmax)))
    Output
      <error/rlang_error>
      Error in `vec_as_short_length()`:
      ! `my_arg` is too large a number and long vectors are not supported.

# vec_size_common() checks inputs

    Code
      (expect_error(vec_size_common(.size = "foo")))
    Output
      <error/rlang_error>
      Error in `vec_size_common()`:
      ! `.size` must be a single number, not a string.
    Code
      (expect_error(vec_size_common(.size = 1:2)))
    Output
      <error/rlang_error>
      Error in `vec_size_common()`:
      ! `.size` must be a single number, not an integer vector of length 2.

# `.absent` must be supplied when `...` is empty

    Code
      (expect_error(vec_size_common(.absent = NULL)))
    Output
      <error/rlang_error>
      Error in `vec_size_common()`:
      ! `.absent` must be supplied when `...` is empty.

# `.absent` must be a length 1 integer if provided

    Code
      (expect_error(vec_size_common(.absent = 1), "must be a single integer"))
    Output
      <error/rlang_error>
      Error in `vec_size_common()`:
      ! `.absent` must be a single integer.
    Code
      (expect_error(vec_size_common(.absent = c(1L, 2L)), "must be a single integer"))
    Output
      <error/rlang_error>
      Error in `vec_size_common()`:
      ! `.absent` must be a single integer.

# argument tags are forwarded

    Code
      vec_size_common(1:2, 1, 1:4)
    Condition
      Error:
      ! Can't recycle `..1` (size 2) to match `..3` (size 4).

---

    Code
      vec_size_common(foo = 1:2, 1, bar = 1:4)
    Condition
      Error:
      ! Can't recycle `foo` (size 2) to match `bar` (size 4).

