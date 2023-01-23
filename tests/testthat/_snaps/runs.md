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

# vec_locate_run_bounds() validates `start`

    Code
      vec_locate_run_bounds(1, start = "x")
    Condition
      Error in `vec_locate_run_bounds()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_locate_run_bounds(1, start = NA)
    Condition
      Error in `vec_locate_run_bounds()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_locate_run_bounds(1, start = c(TRUE, TRUE))
    Condition
      Error in `vec_locate_run_bounds()`:
      ! `start` must be `TRUE` or `FALSE`.

# vec_detect_run_bounds() validates `start`

    Code
      vec_detect_run_bounds(1, start = "x")
    Condition
      Error in `vec_detect_run_bounds()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_detect_run_bounds(1, start = NA)
    Condition
      Error in `vec_detect_run_bounds()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_detect_run_bounds(1, start = c(TRUE, TRUE))
    Condition
      Error in `vec_detect_run_bounds()`:
      ! `start` must be `TRUE` or `FALSE`.

