# casts inputs to logical

    Code
      vec_pall(1.5)
    Condition
      Error in `vec_pall()`:
      ! Can't convert from `..1` <double> to <logical> due to loss of precision.
      * Locations: 1

---

    Code
      vec_pany(1.5)
    Condition
      Error in `vec_pany()`:
      ! Can't convert from `..1` <double> to <logical> due to loss of precision.
      * Locations: 1

# recycles inputs to common size

    Code
      vec_pall(c(TRUE, FALSE), c(TRUE, TRUE, TRUE))
    Condition
      Error in `vec_pall()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      vec_pany(c(TRUE, FALSE), c(TRUE, TRUE, TRUE))
    Condition
      Error in `vec_pany()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

# respects `.size`

    Code
      vec_pall(c(TRUE, FALSE), .size = 3L)
    Condition
      Error in `vec_pall()`:
      ! Can't recycle `..1` (size 2) to size 3.

# validates `.na_rm`

    Code
      vec_pall(.na_rm = c(TRUE, FALSE))
    Condition
      Error in `vec_pall()`:
      ! `.na_rm` must be `TRUE` or `FALSE`.

---

    Code
      vec_pany(.na_rm = c(TRUE, FALSE))
    Condition
      Error in `vec_pany()`:
      ! `.na_rm` must be `TRUE` or `FALSE`.

---

    Code
      vec_pall(.na_rm = 1)
    Condition
      Error in `vec_pall()`:
      ! `.na_rm` must be `TRUE` or `FALSE`.

---

    Code
      vec_pany(.na_rm = 1)
    Condition
      Error in `vec_pany()`:
      ! `.na_rm` must be `TRUE` or `FALSE`.

---

    Code
      vec_pall(.na_rm = NA)
    Condition
      Error in `vec_pall()`:
      ! `.na_rm` must be `TRUE` or `FALSE`.

---

    Code
      vec_pany(.na_rm = NA)
    Condition
      Error in `vec_pany()`:
      ! `.na_rm` must be `TRUE` or `FALSE`.

# validates `.size`

    Code
      vec_pall(.size = c(1, 2))
    Condition
      Error in `vec_pall()`:
      ! `.size` must be a single number, not a double vector of length 2.

---

    Code
      vec_pany(.size = c(1, 2))
    Condition
      Error in `vec_pany()`:
      ! `.size` must be a single number, not a double vector of length 2.

---

    Code
      vec_pall(.size = 1.5)
    Condition
      Error in `vec_pall()`:
      ! `.size` must be a whole number, not a fractional number.

---

    Code
      vec_pany(.size = 1.5)
    Condition
      Error in `vec_pany()`:
      ! `.size` must be a whole number, not a fractional number.

---

    Code
      vec_pall(.size = NA_integer_)
    Condition
      Error in `vec_pall()`:
      ! `.size` must be a single number, not an integer `NA`.

---

    Code
      vec_pany(.size = NA_integer_)
    Condition
      Error in `vec_pany()`:
      ! `.size` must be a single number, not an integer `NA`.

# names are used in errors

    Code
      vec_pall(x = 1.5)
    Condition
      Error in `vec_pall()`:
      ! Can't convert from `x` <double> to <logical> due to loss of precision.
      * Locations: 1

---

    Code
      vec_pany(x = c(TRUE, FALSE), y = logical())
    Condition
      Error in `vec_pany()`:
      ! Can't recycle `x` (size 2) to match `y` (size 0).

