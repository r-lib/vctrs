# allows for name repair

    Code
      vec_interleave(x, x, .name_repair = "unique")
    Message
      New names:
      * `x` -> `x...1`
      * `x` -> `x...2`
    Output
      x...1 x...2 
          1     1 

# can repair names quietly

    Code
      res_unique <- vec_interleave(c(x = 1), c(x = 2), .name_repair = "unique_quiet")
      res_universal <- vec_interleave(c(`if` = 1), c(`in` = 2), .name_repair = "universal_quiet")

# reports type errors

    Code
      vec_interleave(1, "x")
    Condition
      Error in `vec_interleave()`:
      ! Can't combine `..1` <double> and `..2` <character>.

---

    Code
      vec_interleave(1, "x", .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! Can't combine `..1` <double> and `..2` <character>.

---

    Code
      vec_interleave(1, "x", .ptype = double())
    Condition
      Error in `vec_interleave()`:
      ! Can't convert `..2` <character> to <double>.

---

    Code
      vec_interleave(1, "x", .ptype = double(), .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! Can't convert `..2` <character> to <double>.

---

    Code
      vec_interleave(1, NULL, "x")
    Condition
      Error in `vec_interleave()`:
      ! Can't combine `..1` <double> and `..3` <character>.

---

    Code
      vec_interleave(1, NULL, "x", .ptype = double())
    Condition
      Error in `vec_interleave()`:
      ! Can't convert `..3` <character> to <double>.

# reports recycling errors

    Code
      vec_interleave(1:2, 1:3)
    Condition
      Error in `vec_interleave()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      vec_interleave(1:2, 1:3, .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! Can't recycle `..1` (size 2) to match `..2` (size 3).

---

    Code
      vec_interleave(1:2, 3:4, .size = 3)
    Condition
      Error in `vec_interleave()`:
      ! Can't recycle `..1` (size 2) to size 3.

---

    Code
      vec_interleave(1:2, 3:4, .size = 3, .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! Can't recycle `..1` (size 2) to size 3.

---

    Code
      vec_interleave(1:2, NULL, 1:3)
    Condition
      Error in `vec_interleave()`:
      ! Can't recycle `..1` (size 2) to match `..3` (size 3).

---

    Code
      vec_interleave(1:2, NULL, 1:3, .size = 2)
    Condition
      Error in `vec_interleave()`:
      ! Can't recycle `..3` (size 3) to size 2.

# reports scalar errors

    Code
      vec_interleave(lm(1 ~ 1))
    Condition
      Error in `vec_interleave()`:
      ! `..1` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_interleave(lm(1 ~ 1), .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! `..1` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_interleave(1, NULL, lm(1 ~ 1))
    Condition
      Error in `vec_interleave()`:
      ! `..3` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_interleave(1, NULL, lm(1 ~ 1), .error_call = quote(foo()))
    Condition
      Error in `foo()`:
      ! `..3` must be a vector, not a <lm> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

# `list_interleave()` checks for a list

    Code
      list_interleave(1)
    Condition
      Error in `list_interleave()`:
      ! `1` must be a list, not the number 1.

