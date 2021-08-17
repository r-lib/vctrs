# incompatible shapes throw errors

    Code
      vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 5, 1))
    Error <vctrs_error_incompatible_type>
      Can't combine <integer[,0,5]> and <integer[,5,1]>.
      x Incompatible sizes 0 and 5 along axis 2.

---

    Code
      vec_shape2(shaped_int(1, 5, 0), shaped_int(1, 1, 5))
    Error <vctrs_error_incompatible_type>
      Can't combine <integer[,5,0]> and <integer[,1,5]>.
      x Incompatible sizes 0 and 5 along axis 3.

# can override error args

    Code
      vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 5, 1), x_arg = "foo", y_arg = "bar")
    Error <vctrs_error_incompatible_type>
      Can't combine `foo` <integer[,0,5]> and `bar` <integer[,5,1]>.
      x Incompatible sizes 0 and 5 along axis 2.

