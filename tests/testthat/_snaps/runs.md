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

