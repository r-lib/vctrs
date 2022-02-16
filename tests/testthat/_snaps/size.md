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

