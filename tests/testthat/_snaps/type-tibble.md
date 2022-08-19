# can't cast vector to tibble

    Code
      local_error_call(call("my_function"))
      (expect_error(vec_ptype2(v, dt), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `v` <logical> and `dt` <tbl_df>.
    Code
      (expect_error(vec_ptype2(dt, v), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't combine `dt` <tbl_df> and `v` <logical>.
    Code
      (expect_error(vec_cast(v, dt), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error in `my_function()`:
      ! Can't convert `v` <logical> to <tbl_df>.

# can use ptype2 and cast with tibble that has incorrect class vector

    Code
      local_error_call(call("my_function"))
      (expect_error(vec_cast(tib1, tib2), class = "vctrs_error_cast_lossy_dropped"))
    Output
      <error/vctrs_error_cast_lossy_dropped>
      Error in `my_function()`:
      ! Can't convert from `tib1` <tbl_df<x:double>> to <tbl_df<y:double>> due to loss of precision.
    Code
      (expect_error(vec_cast(tib1, data.frame(y = 2)), class = "vctrs_error_cast_lossy_dropped")
      )
    Output
      <error/vctrs_error_cast_lossy_dropped>
      Error in `my_function()`:
      ! Can't convert from `tib1` <tbl_df<x:double>> to <data.frame<y:double>> due to loss of precision.
    Code
      (expect_error(vec_cast(data.frame(x = 1), tib2), class = "vctrs_error_cast_lossy_dropped")
      )
    Output
      <error/vctrs_error_cast_lossy_dropped>
      Error in `my_function()`:
      ! Can't convert from `data.frame(x = 1)` <data.frame<x:double>> to <tbl_df<y:double>> due to loss of precision.

