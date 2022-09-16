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

# can combine shaped native classes (#1290, #1329)

    Code
      vec_c(x, y)
    Condition
      Error:
      ! Can't combine `..1` <datetime<local>> and `..2` <datetime<local>>.
      x Incompatible sizes 2 and 3 along axis 2.

# factor casts support shape

    Code
      vec_cast(x, y)
    Condition
      Error:
      ! Can't convert `x` <factor<32af0>[,1]> to <factor<32af0>>.
      Can't decrease dimensions from 2 to 1.

