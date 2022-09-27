# vec_locate_runs() validates `start`

    Code
      vec_locate_runs(1, start = "x")
    Condition
      Error in `vec_locate_runs()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_locate_runs(1, start = NA)
    Condition
      Error in `vec_locate_runs()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_locate_runs(1, start = c(TRUE, TRUE))
    Condition
      Error in `vec_locate_runs()`:
      ! `start` must be `TRUE` or `FALSE`.

# vec_detect_runs() validates `start`

    Code
      vec_detect_runs(1, start = "x")
    Condition
      Error in `vec_detect_runs()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_detect_runs(1, start = NA)
    Condition
      Error in `vec_detect_runs()`:
      ! `start` must be `TRUE` or `FALSE`.

---

    Code
      vec_detect_runs(1, start = c(TRUE, TRUE))
    Condition
      Error in `vec_detect_runs()`:
      ! `start` must be `TRUE` or `FALSE`.

