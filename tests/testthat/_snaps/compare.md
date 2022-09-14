# error is thrown when comparing complexes (#1655)

    Code
      (expect_error(vec_compare(complex(), complex())))
    Output
      <error/rlang_error>
      Error in `vec_compare()`:
      ! Can't compare complexes with `vec_compare()`.

