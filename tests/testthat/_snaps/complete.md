# catches `NULL` data frame columns

    Code
      vec_detect_complete(df)
    Condition
      Error in `vec_detect_complete()`:
      ! Unexpected `NULL` column found in a data frame.

# catches scalar objects

    Code
      vec_detect_complete(lm(1 ~ 1))
    Condition
      Error in `vec_size()`:
      ! `x` must be a vector, not a <lm> object.

