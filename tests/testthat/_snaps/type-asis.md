# AsIs objects throw ptype2 errors with their underlying types

    Code
      (expect_error(vec_ptype2(I(1), I("x")), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_ptype2>
      Error:
      ! Can't combine `I(1)` <double> and `I("x")` <character>.

# AsIs objects throw cast errors with their underlying types

    Code
      (expect_error(vec_cast(I(1), I(factor("x"))), class = "vctrs_error_incompatible_type")
      )
    Output
      <error/vctrs_error_cast>
      Error:
      ! Can't convert `I(1)` <double> to <factor<bf275>>.

