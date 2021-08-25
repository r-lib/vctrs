# AsIs objects throw ptype2 errors with their underlying types

    Code
      (expect_error(vec_ptype2(I(1), I("x")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Can't combine <double> and <character>.
      Call: `stop_vctrs()`

# AsIs objects throw cast errors with their underlying types

    Code
      (expect_error(vec_cast(I(1), I(factor("x"))), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_incompatible_type>
      Can't convert <double> to <factor<bf275>>.
      Call: `stop_vctrs()`

