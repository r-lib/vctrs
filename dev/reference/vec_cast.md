# Cast a vector to a specified type

`vec_cast()` provides directional conversions from one type of vector to
another. Along with
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md),
this generic forms the foundation of type coercions in vctrs.

## Usage

``` r
vec_cast(x, to, ..., x_arg = caller_arg(x), to_arg = "", call = caller_env())

vec_cast_common(..., .to = NULL, .arg = "", .call = caller_env())

# S3 method for class 'logical'
vec_cast(x, to, ...)

# S3 method for class 'integer'
vec_cast(x, to, ...)

# S3 method for class 'double'
vec_cast(x, to, ...)

# S3 method for class 'complex'
vec_cast(x, to, ...)

# S3 method for class 'raw'
vec_cast(x, to, ...)

# S3 method for class 'character'
vec_cast(x, to, ...)

# S3 method for class 'list'
vec_cast(x, to, ...)
```

## Arguments

- x:

  Vectors to cast.

- to, .to:

  Type to cast to. If `NULL`, `x` will be returned as is.

- ...:

  For `vec_cast_common()`, vectors to cast. For `vec_cast()`,
  `vec_cast_default()`, and
  [`vec_restore()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md),
  these dots are only for future extensions and should be empty.

- x_arg:

  Argument name for `x`, used in error messages to inform the user about
  the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)).

- to_arg:

  Argument name `to` used in error messages to inform the user about the
  locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)).

- call, .call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- .arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

## Value

A vector the same length as `x` with the same type as `to`, or an error
if the cast is not possible. An error is generated if information is
lost when casting between compatible types (i.e. when there is no 1-to-1
mapping for a specific value).

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

## Dependencies of `vec_cast_common()`

### vctrs dependencies

- [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)

- `vec_cast()`

### base dependencies

Some functions enable a base-class fallback for `vec_cast_common()`. In
that case the inputs are deemed compatible when they have the same [base
type](https://rdrr.io/r/base/typeof.html) and inherit from the same base
class.

## See also

Call
[`stop_incompatible_cast()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)
when you determine from the attributes that an input can't be cast to
the target type.

## Examples

``` r
# x is a double, but no information is lost
vec_cast(1, integer())
#> [1] 1

# When information is lost the cast fails
try(vec_cast(c(1, 1.5), integer()))
#> Error in eval(expr, envir) : 
#>   Can't convert from `c(1, 1.5)` <double> to <integer> due to loss of precision.
#> • Locations: 2
try(vec_cast(c(1, 2), logical()))
#> Error in eval(expr, envir) : 
#>   Can't convert from `c(1, 2)` <double> to <logical> due to loss of precision.
#> • Locations: 2

# You can suppress this error and get the partial results
allow_lossy_cast(vec_cast(c(1, 1.5), integer()))
#> [1] 1 1
allow_lossy_cast(vec_cast(c(1, 2), logical()))
#> [1] TRUE TRUE

# By default this suppress all lossy cast errors without
# distinction, but you can be specific about what cast is allowed
# by supplying prototypes
allow_lossy_cast(vec_cast(c(1, 1.5), integer()), to_ptype = integer())
#> [1] 1 1
try(allow_lossy_cast(vec_cast(c(1, 2), logical()), to_ptype = integer()))
#> Error in eval(expr, envir) : 
#>   Can't convert from `c(1, 2)` <double> to <logical> due to loss of precision.
#> • Locations: 2

# No sensible coercion is possible so an error is generated
try(vec_cast(1.5, factor("a")))
#> Error in eval(expr, envir) : 
#>   Can't convert `1.5` <double> to <factor<4d52a>>.

# Cast to common type
vec_cast_common(factor("a"), factor(c("a", "b")))
#> [[1]]
#> [1] a
#> Levels: a b
#> 
#> [[2]]
#> [1] a b
#> Levels: a b
#> 
```
