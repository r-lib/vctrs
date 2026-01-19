# can't unstructure `NULL`

    Code
      vec_unstructure(NULL)
    Condition
      Error in `vec_unstructure()`:
      ! `x` must have a supported storage type, not <NULL>.
      i Read our FAQ about native storage types (`?vctrs::theory_faq_native_storage`) to learn more.

# can't unstructure non-vectors

    Code
      vec_unstructure(environment())
    Condition
      Error in `vec_unstructure()`:
      ! `x` must have a supported storage type, not <environment>.
      i Read our FAQ about native storage types (`?vctrs::theory_faq_native_storage`) to learn more.

