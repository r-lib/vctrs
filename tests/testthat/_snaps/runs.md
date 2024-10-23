# errors on scalars

    Code
      vec_identify_runs(foobar())
    Condition
      Error in `vec_identify_runs()`:
      ! `x` must be a vector, not a <vctrs_foobar> object.

---

    Code
      vec_run_sizes(foobar())
    Condition
      Error in `vec_run_sizes()`:
      ! `x` must be a vector, not a <vctrs_foobar> object.

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

