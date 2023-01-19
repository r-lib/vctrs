# inputs must be named

    Code
      vec_expand_grid(1)
    Condition
      Error in `vec_expand_grid()`:
      ! All inputs must be named.

---

    Code
      vec_expand_grid(x = 1, 2, y = 3)
    Condition
      Error in `vec_expand_grid()`:
      ! All inputs must be named.

# catches duplicate names by default

    Code
      vec_expand_grid(a = 1, a = 2)
    Condition
      Error in `vec_expand_grid()`:
      ! Names must be unique.
      x These names are duplicated:
        * "a" at locations 1 and 2.
      i Use argument `.name_repair` to specify repair strategy.

# errors on non vectors and mentions the element name

    Code
      vec_expand_grid(y = environment())
    Condition
      Error in `vec_expand_grid()`:
      ! `y` must be a vector, not an environment.

# can adjust the `.error_call`

    Code
      my_expand_grid()
    Condition
      Error in `my_expand_grid()`:
      ! `x` must be a vector, not an environment.

# errors nicely when expansion results in a size larger than `R_len_t`

    Code
      vec_expand_grid(x = x, y = y)
    Condition
      Error in `vec_expand_grid()`:
      ! Long vectors are not yet supported. Expansion results in an allocation larger than 2^31-1 elements. Attempted allocation size was 3221225469.

# errors nicely when expansion results in a size larger than `R_xlen_t`

    Code
      vec_expand_grid(x = x, y = x)
    Condition
      Error in `vec_expand_grid()`:
      ! Result too large for an `r_ssize`.
      i In file './rlang/c-utils.h' at line <scrubbed>.
      i This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.

# validates `.vary`

    Code
      vec_expand_grid(.vary = 1)
    Condition
      Error in `vec_expand_grid()`:
      ! `.vary` must be a string or character vector.

---

    Code
      vec_expand_grid(.vary = "x")
    Condition
      Error in `vec_expand_grid()`:
      ! `.vary` must be one of "slowest" or "fastest", not "x".

