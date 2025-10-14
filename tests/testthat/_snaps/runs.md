# errors on scalars

    Code
      vec_identify_runs(foobar())
    Condition
      Error in `vec_identify_runs()`:
      ! `x` must be a vector, not a <vctrs_foobar> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <vctrs_foobar>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

---

    Code
      vec_run_sizes(foobar())
    Condition
      Error in `vec_run_sizes()`:
      ! `x` must be a vector, not a <vctrs_foobar> object.
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <vctrs_foobar>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?howto_faq_fix_scalar_type_error`) to learn more.

# vec_locate_run_bounds() validates `which`

    Code
      vec_locate_run_bounds(1, which = "x")
    Condition
      Error in `vec_locate_run_bounds()`:
      ! `which` must be one of "start" or "end", not "x".

---

    Code
      vec_locate_run_bounds(1, which = 1)
    Condition
      Error in `vec_locate_run_bounds()`:
      ! `which` must be a string or character vector.

---

    Code
      vec_locate_run_bounds(1, which = c("foo", "bar"))
    Condition
      Error in `vec_locate_run_bounds()`:
      ! `arg` must be length 1 or a permutation of `c("start", "end")`.

# vec_detect_run_bounds() validates `which`

    Code
      vec_detect_run_bounds(1, which = "x")
    Condition
      Error in `vec_detect_run_bounds()`:
      ! `which` must be one of "start" or "end", not "x".

---

    Code
      vec_detect_run_bounds(1, which = 1)
    Condition
      Error in `vec_detect_run_bounds()`:
      ! `which` must be a string or character vector.

---

    Code
      vec_detect_run_bounds(1, which = c("foo", "bar"))
    Condition
      Error in `vec_detect_run_bounds()`:
      ! `arg` must be length 1 or a permutation of `c("start", "end")`.

