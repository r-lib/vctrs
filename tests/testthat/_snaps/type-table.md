# cannot decrease dimensionality

    Code
      (expect_error(vec_cast(x, y), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't convert `x` <table[,1,1]> to <table[,1]>.
      Can't decrease dimensionality from 3 to 2.

