# incompatible shapes throw errors

    Code
      (expect_error(vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 5, 1)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine <integer[,0,5]> and <integer[,5,1]>.
      x Incompatible sizes 0 and 5 along axis 2.
    Code
      (expect_error(vec_shape2(shaped_int(1, 5, 0), shaped_int(1, 1, 5)), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine <integer[,5,0]> and <integer[,1,5]>.
      x Incompatible sizes 0 and 5 along axis 3.

# can override error args

    Code
      (expect_error(vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 5, 1), x_arg = "foo",
      y_arg = "bar"), class = "vctrs_error_incompatible_type"))
    Output
      <error/vctrs_error_incompatible_type>
      Error:
      ! Can't combine `foo` <integer[,0,5]> and `bar` <integer[,5,1]>.
      x Incompatible sizes 0 and 5 along axis 2.

