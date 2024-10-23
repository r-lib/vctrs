# vec_unchop() is soft-deprecated

    Code
      vec_unchop(list(1), indices = list(1))
    Condition
      Warning:
      `vec_unchop()` was deprecated in vctrs 0.5.0.
      i Please use `list_unchop()` instead.
    Output
      [1] 1

# vec_equal_na() is soft-deprecated

    Code
      vec_equal_na(c(1, NA))
    Condition
      Warning:
      `vec_equal_na()` was deprecated in vctrs 0.5.0.
      i Please use `vec_detect_missing()` instead.
    Output
      [1] FALSE  TRUE

# vec_check_list() still works

    Code
      vec_check_list(1)
    Condition
      Error:
      ! `1` must be a list, not the number 1.

---

    Code
      my_check(1)
    Condition
      Error in `my_check()`:
      ! `x` must be a list, not the number 1.

