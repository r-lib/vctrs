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
      x Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <lm>.
      i If this object comes from a package, please report this error to the package author.
      i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

