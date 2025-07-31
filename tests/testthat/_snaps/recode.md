# `x` and `from` common type errors are correct

    Code
      vec_recode_values(1, from = "a", to = 1)
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `from` <character> to match type of `x` <double>.

---

    Code
      vec_recode_values(1, from = list("a"), to = 1, multiple_from = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `from[[1]]` <character> to <double>.

# `to` and `default` `ptype` errors are correct when it is inferred

    Code
      vec_recode_values(1, from = 1:2, to = list(1, "x"), multiple_to = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't combine `to[[1]]` <double> and `to[[2]]` <character>.

---

    Code
      vec_recode_values(1, from = 1:2, to = list(1, 2), default = "x", multiple_to = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't combine `default` <character> and <double>.

---

    Code
      vec_recode_values(1, from = 1:2, to = 1, default = "x")
    Condition
      Error in `vec_recode_values()`:
      ! Can't combine `to` <double> and `default` <character>.

# `to` and `default` `ptype` errors are correct when it is user supplied

    Code
      vec_recode_values(1, from = 1, to = 1, ptype = foobar())
    Condition
      Error in `vec_recode_values()`:
      ! `ptype` must be a vector, not a <vctrs_foobar> object.

---

    Code
      vec_recode_values(1, from = 1, to = 1, ptype = character())
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `to` <double> to <character>.

---

    Code
      vec_recode_values(1, from = 1, to = list(a = 1), ptype = character(),
      multiple_to = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `to$a` <double> to <character>.

---

    Code
      vec_recode_values(1, from = 1, to = "x", default = 1, ptype = character())
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `default` <double> to <character>.

# `to` size is validated

    Code
      vec_recode_values(1:5, from = 1, to = 2:3)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to` (size 2) to size 1.

---

    Code
      vec_recode_values(1:5, from = list(1), to = 2:3, multiple_from = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to` (size 2) to size 1.

---

    Code
      vec_recode_values(1:5, from = 1, to = list(2, 3), multiple_to = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! `to` must have size 1, not size 2.

---

    Code
      vec_recode_values(1:5, from = list(1), to = list(2, 3), multiple_from = TRUE,
      multiple_to = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! `to` must have size 1, not size 2.

---

    Code
      vec_recode_values(1:5, from = 1, to = list(a = 2:3), multiple_to = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to$a` (size 2) to size 5.

# `default` size is validated

    Code
      vec_recode_values(1:5, from = 1, to = 2, default = 1:2)
    Condition
      Error in `vec_recode_values()`:
      ! `default` must have size 5, not size 2.

# `x` must be a vector

    Code
      vec_recode_values(foobar(), from = 1, to = 2, x_arg = ".x")
    Condition
      Error in `vec_recode_values()`:
      ! `.x` must be a vector, not a <vctrs_foobar> object.

# `from` must be a vector or list of vectors

    Code
      vec_recode_values(1, from = foobar(), to = 2, from_arg = ".from")
    Condition
      Error in `vec_recode_values()`:
      ! `.from` must be a vector, not a <vctrs_foobar> object.

---

    Code
      vec_recode_values(1, from = 1, to = 2, multiple_from = TRUE, from_arg = ".from")
    Condition
      Error in `vec_recode_values()`:
      ! `.from` must be a list, not the number 1.

---

    Code
      vec_recode_values(1, from = list(a = foobar()), to = 2, multiple_from = TRUE,
      from_arg = ".from")
    Condition
      Error in `vec_recode_values()`:
      ! `.from$a` must be a vector, not a <vctrs_foobar> object.

# `to` must be a vector or list of vectors

    Code
      vec_recode_values(1, from = 1, to = foobar(), to_arg = ".to")
    Condition
      Error in `vec_recode_values()`:
      ! `.to` must be a vector, not a <vctrs_foobar> object.

---

    Code
      vec_recode_values(1, from = 1, to = 2, multiple_to = TRUE, to_arg = ".to")
    Condition
      Error in `vec_recode_values()`:
      ! `.to` must be a list, not the number 2.

---

    Code
      vec_recode_values(1, from = 1, to = list(a = foobar()), multiple_to = TRUE,
      to_arg = ".to")
    Condition
      Error in `vec_recode_values()`:
      ! `.to$a` must be a vector, not a <vctrs_foobar> object.

# `default` must be a vector

    Code
      vec_recode_values(1, from = 1, to = 2, default = foobar(), default_arg = ".default")
    Condition
      Error in `vec_recode_values()`:
      ! `.default` must be a vector, not a <vctrs_foobar> object.

# `multiple_from` and `multiple_to` are validated

    Code
      vec_recode_values(1, from = 1, to = 1, multiple_from = "x")
    Condition
      Error in `vec_recode_values()`:
      ! `multiple_from` must be `TRUE` or `FALSE`.

---

    Code
      vec_recode_values(1, from = 1, to = 1, multiple_to = "x")
    Condition
      Error in `vec_recode_values()`:
      ! `multiple_to` must be `TRUE` or `FALSE`.

---

    Code
      vec_replace_values(1, from = 1, to = 1, multiple_from = "x")
    Condition
      Error in `vec_recode_values()`:
      ! `multiple_from` must be `TRUE` or `FALSE`.

---

    Code
      vec_replace_values(1, from = 1, to = 1, multiple_to = "x")
    Condition
      Error in `vec_recode_values()`:
      ! `multiple_to` must be `TRUE` or `FALSE`.

# `unmatched` is validated

    Code
      vec_recode_values(1, from = 1, to = 1, unmatched = "e")
    Condition
      Error in `vec_recode_values()`:
      ! `unmatched` must be either "default" or "error".

# proof that `ptype` finalization is important

    Code
      vec_recode_values(x, from = from, to = to, default = x, ptype = x)
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `to` <logical> to <vctrs_unspecified>.

