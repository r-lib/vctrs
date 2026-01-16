# Convert to a base subscript type

**\[experimental\]**

Convert `i` to the base type expected by
[`vec_as_location()`](https://vctrs.r-lib.org/reference/vec_as_location.md)
or
[`vec_as_location2()`](https://vctrs.r-lib.org/reference/vec_as_location.md).
The values of the subscript type are not checked in any way (length,
missingness, negative elements).

## Usage

``` r
vec_as_subscript(
  i,
  ...,
  logical = c("cast", "error"),
  numeric = c("cast", "error"),
  character = c("cast", "error"),
  arg = NULL,
  call = caller_env()
)

vec_as_subscript2(
  i,
  ...,
  numeric = c("cast", "error"),
  character = c("cast", "error"),
  arg = NULL,
  call = caller_env()
)
```

## Arguments

- i:

  An index vector to convert.

- ...:

  These dots are for future extensions and must be empty.

- logical, numeric, character:

  How to handle logical, numeric, and character subscripts.

  If `"cast"` and the subscript is not one of the three base types
  (logical, integer or character), the subscript is
  [cast](https://vctrs.r-lib.org/reference/vec_cast.md) to the relevant
  base type, e.g. factors are coerced to character. `NULL` is treated as
  an empty integer vector, and is thus coercible depending on the
  setting of `numeric`. Symbols are treated as character vectors and
  thus coercible depending on the setting of `character`.

  If `"error"`, the subscript type is disallowed and triggers an
  informative error.

- arg:

  The argument name to be displayed in error messages.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
