# `unmatched` errors are correct

    Code
      vec_recode_values(c(1, 2), from = 1, to = 0, unmatched = "error")
    Condition
      Error in `vec_recode_values()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      vec_recode_values(c(1, NA), from = 1, to = 0, unmatched = "error")
    Condition
      Error in `vec_recode_values()`:
      ! Each location must be matched.
      x Location 2 is unmatched.

---

    Code
      vec_recode_values(1:100, from = 1, to = 0, unmatched = "error")
    Condition
      Error in `vec_recode_values()`:
      ! Each location must be matched.
      x Locations 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, ..., 99, and 100 are unmatched.

# `x` and `from` common type errors are correct

    Code
      vec_recode_values(1, from = "a", to = 1)
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `from` <character> to match type of `x` <double>.

---

    Code
      vec_recode_values(1, from = list("a"), to = 1, from_as_list_of_vectors = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't convert `from[[1]]` <character> to <double>.

# `to` and `default` `ptype` errors are correct when it is inferred

    Code
      vec_recode_values(1, from = 1:2, to = list(1, "x"), to_as_list_of_vectors = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't combine `to[[1]]` <double> and `to[[2]]` <character>.

---

    Code
      vec_recode_values(1, from = 1:2, to = list(1, 2), default = "x",
      to_as_list_of_vectors = TRUE)
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
      to_as_list_of_vectors = TRUE)
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
      vec_recode_values(1:5, from = list(1), to = 2:3, from_as_list_of_vectors = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to` (size 2) to size 1.

---

    Code
      vec_recode_values(1:5, from = 1, to = list(2, 3), to_as_list_of_vectors = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to` (size 2) to size 1.

---

    Code
      vec_recode_values(1:5, from = list(1), to = list(2, 3),
      from_as_list_of_vectors = TRUE, to_as_list_of_vectors = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to` (size 2) to size 1.

---

    Code
      vec_recode_values(1:5, from = 1, to = list(a = 2:3), to_as_list_of_vectors = TRUE)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `to$a` (size 2) to size 5.

# `default` size is validated

    Code
      vec_recode_values(1:5, from = 1, to = 2, default = 1:2)
    Condition
      Error in `vec_recode_values()`:
      ! Can't recycle `default` (size 2) to size 5.

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
      vec_recode_values(1, from = 1, to = 2, from_as_list_of_vectors = TRUE,
        from_arg = ".from")
    Condition
      Error in `vec_recode_values()`:
      ! `.from` must be a list, not the number 1.

---

    Code
      vec_recode_values(1, from = list(a = foobar()), to = 2,
      from_as_list_of_vectors = TRUE, from_arg = ".from")
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
      vec_recode_values(1, from = 1, to = 2, to_as_list_of_vectors = TRUE, to_arg = ".to")
    Condition
      Error in `vec_recode_values()`:
      ! `.to` must be a list, not the number 2.

---

    Code
      vec_recode_values(1, from = 1, to = list(a = foobar()), to_as_list_of_vectors = TRUE,
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

# `from_as_list_of_vectors` and `to_as_list_of_vectors` are validated

    Code
      vec_recode_values(1, from = 1, to = 1, from_as_list_of_vectors = "x")
    Condition
      Error in `vec_recode_values()`:
      ! `from_as_list_of_vectors` must be `TRUE` or `FALSE`.

---

    Code
      vec_recode_values(1, from = 1, to = 1, to_as_list_of_vectors = "x")
    Condition
      Error in `vec_recode_values()`:
      ! `to_as_list_of_vectors` must be `TRUE` or `FALSE`.

---

    Code
      vec_replace_values(1, from = 1, to = 1, from_as_list_of_vectors = "x")
    Condition
      Error in `vec_replace_values()`:
      ! `from_as_list_of_vectors` must be `TRUE` or `FALSE`.

---

    Code
      vec_replace_values(1, from = 1, to = 1, to_as_list_of_vectors = "x")
    Condition
      Error in `vec_replace_values()`:
      ! `to_as_list_of_vectors` must be `TRUE` or `FALSE`.

# `unmatched` is validated

    Code
      vec_recode_values(1, from = 1, to = 1, unmatched = "e")
    Condition
      Error in `vec_recode_values()`:
      ! `unmatched` must be either "default" or "error", not "e".

