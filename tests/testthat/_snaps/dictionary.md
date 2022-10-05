# vec_match() and vec_in() check types

    Code
      df1 <- data_frame(x = data_frame(foo = 1))
      df2 <- data_frame(x = data_frame(foo = ""))
      (expect_error(vec_match(df1, df2), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_match()`:
      ! Can't combine `x$foo` <double> and `x$foo` <character>.
    Code
      (expect_error(vec_match(df1, df2, needles_arg = "n", haystack_arg = "h"),
      class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_match()`:
      ! Can't combine `n$x$foo` <double> and `h$x$foo` <character>.
    Code
      (expect_error(vec_in(df1, df2), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_in()`:
      ! Can't combine `x$foo` <double> and `x$foo` <character>.
    Code
      (expect_error(vec_in(df1, df2, needles_arg = "n", haystack_arg = "h"), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error in `vec_in()`:
      ! Can't combine `n$x$foo` <double> and `h$x$foo` <character>.

