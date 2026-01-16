# FAQ - Error/Warning: Some attributes are incompatible

This error occurs when
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) or
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) are
supplied vectors of the same classes with different attributes. In this
case, vctrs doesn't know how to combine the inputs.

To fix this error, the maintainer of the class should implement
self-to-self coercion methods for
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) and
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md).

## Implementing coercion methods

- For an overview of how these generics work and their roles in vctrs,
  see
  [`?theory-faq-coercion`](https://vctrs.r-lib.org/reference/theory-faq-coercion.md).

- For an example of implementing coercion methods for simple vectors,
  see
  [`?howto-faq-coercion`](https://vctrs.r-lib.org/reference/howto-faq-coercion.md).

- For an example of implementing coercion methods for data frame
  subclasses, see
  [`?howto-faq-coercion-data-frame`](https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.md).

- For a tutorial about implementing vctrs classes from scratch, see
  [`vignette("s3-vector")`](https://vctrs.r-lib.org/articles/s3-vector.md).
