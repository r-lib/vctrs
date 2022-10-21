# data.table and tibble do not have a common type

    Code
      (expect_error(vec_cast(tibble(y = 2), data.table(x = TRUE, y = 1L))))
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert `tibble(y = 2)` <tibble> to <data.table>.

# data table has formatting methods

    Code
      dt <- data.table(x = 1, y = 2, z = 3)
      vec_ptype_abbr(dt)
    Output
      [1] "dt[,3]"
    Code
      vec_ptype_full(dt)
    Output
      [1] "data.table<\n  x: double\n  y: double\n  z: double\n>"

