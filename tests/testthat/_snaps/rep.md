# `vec_rep()` validates `times`

    Code
      vec_rep(1, "x")
    Condition
      Error in `stop_vctrs()`:
      ! Can't convert `times` <character> to <integer>.

---

    Code
      vec_rep(1, c(1, 2))
    Condition
      Error:
      ! `times` must be a single number.

---

    Code
      vec_rep(1, -1)
    Condition
      Error:
      ! `times` must be a positive number.

---

    Code
      vec_rep(1, NA_integer_)
    Condition
      Error:
      ! `times` can't be missing.

# `vec_rep_each()` validates `times`

    Code
      vec_rep_each(1, "x")
    Condition
      Error in `stop_vctrs()`:
      ! Can't convert `times` <character> to <integer>.

---

    Code
      vec_rep_each(1, -1)
    Condition
      Error:
      ! `times` must be a vector of positive numbers. Location 1 is negative.

---

    Code
      vec_rep_each(c(1, 2), c(1, -1))
    Condition
      Error:
      ! `times` must be a vector of positive numbers. Location 2 is negative.

---

    Code
      vec_rep_each(1, NA_integer_)
    Condition
      Error:
      ! `times` can't be missing. Location 1 is missing.

---

    Code
      vec_rep_each(c(1, 2), c(1, NA_integer_))
    Condition
      Error:
      ! `times` can't be missing. Location 2 is missing.

# `vec_rep_each()` uses recyclying errors

    Code
      vec_rep_each(1:2, 1:3)
    Condition
      Error in `stop_vctrs()`:
      ! Can't recycle `times` (size 3) to size 2.

