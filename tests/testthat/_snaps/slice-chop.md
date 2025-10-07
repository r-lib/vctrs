# `indices` are validated

    Code
      vec_chop(1, indices = 1)
    Condition
      Error:
      ! `indices` must be a list of index values, or `NULL`.

---

    Code
      (expect_error(vec_chop(1, indices = list(1.5)), class = "vctrs_error_subscript_type")
      )
    Output
      <error/vctrs_error_subscript_type>
      Error:
      ! Can't subset elements.
      x Can't convert from <double> to <integer> due to loss of precision.

---

    Code
      (expect_error(vec_chop(1, indices = list(2)), class = "vctrs_error_subscript_oob")
      )
    Output
      <error/vctrs_error_subscript_oob>
      Error:
      ! Can't subset elements past the end.
      i Location 2 doesn't exist.
      i There is only 1 element.

# `sizes` are validated

    Code
      vec_chop("a", sizes = "a")
    Condition
      Error:
      ! Can't convert `sizes` <character> to <integer>.

---

    Code
      vec_chop("a", sizes = 2)
    Condition
      Error:
      ! `sizes` can't contain sizes larger than 1.

---

    Code
      vec_chop("a", sizes = -1)
    Condition
      Error:
      ! `sizes` can't contain negative sizes.

---

    Code
      vec_chop("a", sizes = NA_integer_)
    Condition
      Error:
      ! `sizes` can't contain missing values.

---

    Code
      vec_chop("a", sizes = c(1, 1))
    Condition
      Error:
      ! `sizes` must sum to size 1, not size 2.

# can't use both `indices` and `sizes`

    Code
      vec_chop(1, indices = list(1), sizes = 1)
    Condition
      Error:
      ! Can't supply both `indices` and `sizes`.

# `vec_chop(x, indices)` backwards compatible behavior works

    Code
      vec_chop(1:2, 1)
    Condition
      Error:
      ! `indices` must be a list of index values, or `NULL`.

---

    Code
      vec_chop(1, list(1), sizes = 1)
    Condition
      Error:
      ! Can't supply both `indices` and `sizes`.

---

    Code
      vec_chop(1, list(1), 2)
    Condition
      Error in `vec_chop()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = list(1)
      * ..2 = 2
      i Did you forget to name an argument?

---

    Code
      vec_chop(1, list(1), indices = list(1))
    Condition
      Error in `vec_chop()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = list(1)
      i Did you forget to name an argument?

# `vec_chop()` can't take `compact_seq()` indices directly

    Code
      vec_chop(1:2, indices = list(compact_seq(1, 2)))
    Condition
      Error in `vec_chop()`:
      ! `compact_seq` are not allowed.
      i In file 'slice-chop.c' at line <scrubbed>.
      i This is an internal error that was detected in the vctrs package.
        Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

