# A 1d vector of unspecified type

This is the underlying type used to represent logical vectors that only
contain `NA`. These require special handling because we want to allow
logical `NA` to specify missingness that can be cast to any other type.

[`vec_ptype()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md) and
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
convert a logical vector of `NA` into an empty `<unspecified>` type.
This type can combine with any other type.

[`vec_ptype_common()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md)
uses both
[`vec_ptype()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md) and
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md) to
compute the common type, but then returns a *finalised* type using
`vec_ptype_finalise()`. The purpose of `vec_ptype_finalise()` is to turn
any remaining `<unspecified>` types back into `<logical>`, which is the
more useful type for callers of
[`vec_ptype_common()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md).

`vec_ptype_finalise()` is an S3 generic, but it is extremely rare to
need to write an S3 method for this. Data frames (and data frame
subclasses) are already recursively finalised by the default method. The
only time you may need to write an S3 method for `vec_ptype_finalise()`
is if your class *wraps* an arbitrary vector that has the potential to
be a logical vector containing only `NA`s. See `ivs::iv()` for an
example of this, which wraps arbitrary `start` and `end` vectors of the
same type into a single interval vector class.

## Usage

``` r
unspecified(n = 0)

vec_ptype_finalise(x, ...)
```

## Arguments

- n:

  Length of vector

- x:

  A `ptype` to finalize, typically a result of
  [`vec_ptype()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md) or
  [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md).

- ...:

  These dots are for future extensions and must be empty.

## Examples

``` r
# Returns `unspecified()`
vec_ptype(NA)
#> <unspecified> [0]
vec_ptype(c(NA, NA))
#> <unspecified> [0]

# We've chosen to make this return `logical()`, but this is admittedly
# ambiguous, as it could be seen as "an empty vector of `NA`s" that could
# also be treated as unspecified.
vec_ptype(logical())
#> logical(0)

# These return `unspecified()`
vec_ptype2(NA, NA)
#> <unspecified> [0]
vec_ptype2(NA, NULL)
#> <unspecified> [0]
vec_ptype2(NULL, NA)
#> <unspecified> [0]

# An unspecified vector can combine with any other type
vec_ptype2(NA, "x")
#> character(0)
vec_ptype2("x", NA)
#> character(0)

# Same as using `unspecified()` directly
vec_ptype2(unspecified(1), "x")
#> character(0)
vec_ptype2("x", unspecified(1))
#> character(0)

# Finalising a ptype turns unspecified back to logical
vec_ptype(NA)
#> <unspecified> [0]
vec_ptype_finalise(vec_ptype(NA))
#> logical(0)

# This works recursively over data frames
df <- data_frame(x = NA, y = data_frame(z = NA))
vec_ptype_show(vec_ptype(df))
#> Prototype: data.frame<
#>   x: vctrs_unspecified
#>   y: data.frame<z:vctrs_unspecified>
#> >
vec_ptype_show(vec_ptype_finalise(vec_ptype(df)))
#> Prototype: data.frame<
#>   x: logical
#>   y: data.frame<z:logical>
#> >

# `vec_ptype_common()` finalises automatically rather than returning an
# unspecified type
vec_ptype_common(NA)
#> logical(0)
vec_ptype_common(NA, NA)
#> logical(0)
vec_ptype_show(vec_ptype_common(df))
#> Prototype: data.frame<
#>   x: logical
#>   y: data.frame<z:logical>
#> >
```
