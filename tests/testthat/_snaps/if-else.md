# `condition` must be a condition vector

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a vector, not a <lm> object.
      x Detected incompatible S3 list. To be a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a vector, not a <lm> object.
      x Detected incompatible S3 list. To be a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not the number 1.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not the number 1.

---

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not a <foo> object.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not a <foo> object.

---

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not a logical 1D array.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not a logical 1D array.

# `true`, `false`, and `missing` must be vectors

    Code
      vec_if_else(condition = TRUE, true = lm(1 ~ 1), false = 2, missing = 0)
    Condition
      Error in `vec_if_else()`:
      ! `true` must be a vector, not a <lm> object.
      x Detected incompatible S3 list. To be a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_if_else(condition = TRUE, true = 1, false = lm(1 ~ 1), missing = 0)
    Condition
      Error in `vec_if_else()`:
      ! `false` must be a vector, not a <lm> object.
      x Detected incompatible S3 list. To be a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_if_else(condition = TRUE, true = 1, false = 2, missing = lm(1 ~ 1))
    Condition
      Error in `vec_if_else()`:
      ! `missing` must be a vector, not a <lm> object.
      x Detected incompatible S3 list. To be a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

# `true`, `false`, and `missing` must recycle to size of `condition`

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `true` (size 2) to size 1.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `true` (size 2) to size 1.

---

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `false` (size 2) to size 1.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `false` (size 2) to size 1.

---

    Code
      vec_if_else(condition = condition, true = true, false = false, missing = missing,
        ptype = ptype, condition_arg = condition_arg, true_arg = true_arg, false_arg = false_arg,
        missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `missing` (size 2) to size 1.

---

    Code
      vec_if_else(condition = condition, true = true_vctr, false = false_vctr,
        missing = missing_vctr, ptype = ptype, condition_arg = condition_arg,
        true_arg = true_arg, false_arg = false_arg, missing_arg = missing_arg)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `missing` (size 2) to size 1.

# `ptype` overrides common type

    Code
      vec_if_else(condition = TRUE, true = 1.5, false = 2, missing = 0, ptype = integer())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert from `true` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_if_else(condition = TRUE, true = 1, false = 2.5, missing = 0, ptype = integer())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert from `false` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_if_else(condition = TRUE, true = 1, false = 2, missing = 0.5, ptype = integer())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert from `missing` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      vec_if_else(condition = TRUE, true = 1, false = new_date(2), missing = new_date(
        0), ptype = new_date())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert `true` <double> to <date>.

---

    Code
      vec_if_else(condition = TRUE, true = new_date(1), false = 2, missing = new_date(
        0), ptype = new_date())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert `false` <double> to <date>.

---

    Code
      vec_if_else(condition = TRUE, true = new_date(1), false = new_date(2), missing = 0,
      ptype = new_date())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert `missing` <double> to <date>.

# takes the common type of `true` and `false` (tidyverse/dplyr#6243)

    Code
      vec_if_else(TRUE, 1, "x")
    Condition
      Error in `vec_if_else()`:
      ! Can't combine `true` <double> and `false` <character>.

---

    Code
      vec_if_else(TRUE, 1, "x", true_arg = "t", false_arg = "f", error_call = current_env())
    Condition
      Error:
      ! Can't combine `t` <double> and `f` <character>.

# includes `missing` in the common type computation if used

    Code
      vec_if_else(TRUE, 1, 2, missing = "x")
    Condition
      Error in `vec_if_else()`:
      ! Can't combine `true` <double> and `missing` <character>.

---

    Code
      vec_if_else(TRUE, 1L, 2, missing = "x")
    Condition
      Error in `vec_if_else()`:
      ! Can't combine `false` <double> and `missing` <character>.

---

    Code
      vec_if_else(TRUE, 1, 2L, missing = "x")
    Condition
      Error in `vec_if_else()`:
      ! Can't combine `true` <double> and `missing` <character>.

# `condition` must be logical (and isn't cast to logical!)

    Code
      vec_if_else(1:10, 1, 2)
    Condition
      Error in `vec_if_else()`:
      ! `condition` must be a logical vector, not an integer vector.

# `true`, `false`, and `missing` must recycle to the size of `condition`

    Code
      vec_if_else(x < 2, bad, x)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `true` (size 2) to size 3.

---

    Code
      vec_if_else(x < 2, x, bad)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `false` (size 2) to size 3.

---

    Code
      vec_if_else(x < 2, x, x, missing = bad)
    Condition
      Error in `vec_if_else()`:
      ! Can't recycle `missing` (size 2) to size 3.

# must have empty dots

    Code
      vec_if_else(TRUE, 1, 2, missing = 3, 4)
    Condition
      Error in `vec_if_else()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 4
      i Did you forget to name an argument?

# `ptype` overrides the common type

    Code
      vec_if_else(TRUE, 1L, 2.5, ptype = integer())
    Condition
      Error in `vec_if_else()`:
      ! Can't convert from `false` <double> to <integer> due to loss of precision.
      * Locations: 1

