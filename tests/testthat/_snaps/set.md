# errors nicely if common type can't be taken

    Code
      vec_set_intersect(1, "x")
    Condition
      Error in `vec_set_intersect()`:
      ! Can't combine `x` <double> and `y` <character>.

# dots must be empty

    Code
      vec_set_intersect(1, 2, 3)
    Condition
      Error in `vec_set_intersect()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 3
      i Did you forget to name an argument?

# `ptype` is respected

    Code
      vec_set_intersect(1, 1.5, ptype = integer())
    Condition
      Error in `vec_set_intersect()`:
      ! Can't convert from `y` <double> to <integer> due to loss of precision.
      * Locations: 1

# `x_arg` and `y_arg` can be adjusted

    Code
      vec_set_intersect(1, "2", x_arg = "foo", y_arg = "bar")
    Condition
      Error in `vec_set_intersect()`:
      ! Can't combine `foo` <double> and `bar` <character>.

---

    Code
      vec_set_intersect(1, "2", x_arg = "", y_arg = "")
    Condition
      Error in `vec_set_intersect()`:
      ! Can't combine <double> and <character>.

# `error_call` can be adjusted

    Code
      my_set_intersect()
    Condition
      Error in `my_set_intersect()`:
      ! Can't combine `x` <double> and `y` <character>.

