# Find the common type for a pair of vectors

`vec_ptype2()` defines the coercion hierarchy for a set of related
vector types. Along with
[`vec_cast()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md), this
generic forms the foundation of type coercions in vctrs.

`vec_ptype2()` is relevant when you are implementing vctrs methods for
your class, but it should not usually be called directly. If you need to
find the common type of a set of inputs, call
[`vec_ptype_common()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md)
instead. This function supports multiple inputs and
[finalises](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
the common type.

## Usage

``` r
# S3 method for class 'logical'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'integer'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'double'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'complex'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'character'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'raw'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'list'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

vec_ptype2(
  x,
  y,
  ...,
  x_arg = caller_arg(x),
  y_arg = caller_arg(y),
  call = caller_env()
)
```

## Arguments

- x, y:

  Vector types.

- ...:

  These dots are for future extensions and must be empty.

- x_arg, y_arg:

  Argument names for `x` and `y`. These are used in error messages to
  inform the user about the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Implementing coercion methods

- For an overview of how these generics work and their roles in vctrs,
  see
  [`?theory-faq-coercion`](https://vctrs.r-lib.org/dev/reference/theory-faq-coercion.md).

- For an example of implementing coercion methods for simple vectors,
  see
  [`?howto-faq-coercion`](https://vctrs.r-lib.org/dev/reference/howto-faq-coercion.md).

- For an example of implementing coercion methods for data frame
  subclasses, see
  [`?howto-faq-coercion-data-frame`](https://vctrs.r-lib.org/dev/reference/howto-faq-coercion-data-frame.md).

- For a tutorial about implementing vctrs classes from scratch, see
  [`vignette("s3-vector")`](https://vctrs.r-lib.org/dev/articles/s3-vector.md).

## Dependencies

- [`vec_ptype()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md) is
  applied to `x` and `y`

## See also

[`stop_incompatible_type()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)
when you determine from the attributes that an input can't be cast to
the target type.
