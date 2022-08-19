# vec_recycle_common() reports error context

    Code
      (expect_error(my_function(this_arg = 1:2, that_arg = int())))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `my_function()`:
      ! Can't recycle `this_arg` (size 2) to match `that_arg` (size 0).
    Code
      (expect_error(my_function(this_arg = 1:2, that_arg = int(), .size = 2)))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `my_function()`:
      ! Can't recycle `that_arg` (size 0) to size 2.
    Code
      (expect_error(my_function(this_arg = 1:2, that_arg = int(), .arg = "my_arg")))
    Output
      <error/vctrs_error_incompatible_size>
      Error in `my_function()`:
      ! Can't recycle `my_arg$this_arg` (size 2) to match `my_arg$that_arg` (size 0).
    Code
      (expect_error(my_function(this_arg = 1:2, that_arg = int(), .size = 2, .arg = "my_arg"))
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error in `my_function()`:
      ! Can't recycle `my_arg$that_arg` (size 0) to size 2.

# vec_recycle(): incompatible lengths get error messages

    Code
      (expect_error(vec_recycle(x2, 1), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle input of size 2 to size 1.

# vec_recycle_common(): incompatible lengths get error messages

    Code
      (expect_error(vec_recycle_common(1:2, 1:3), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

# recycling matrices respects incompatible sizes

    Code
      (expect_error(vec_recycle_common(x2, x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `..1` (size 2) to match `..2` (size 4).

# recycling data frames respects incompatible sizes

    Code
      (expect_error(vec_recycle_common(x2, x), class = "vctrs_error_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

# recycling to size 1 has informative error

    Code
      (expect_error(vec_recycle(1:2, 1), class = "vctrs_error_recycle_incompatible_size")
      )
    Output
      <error/vctrs_error_incompatible_size>
      Error:
      ! Can't recycle input of size 2 to size 1.

# incompatible recycling size has informative error

    Code
      vec_recycle(1:2, 4)
    Condition
      Error:
      ! Can't recycle input of size 2 to size 4.

---

    Code
      vec_recycle(1:2, 4, x_arg = "foo")
    Condition
      Error:
      ! Can't recycle `foo` (size 2) to size 4.

