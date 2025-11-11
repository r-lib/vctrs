# Vector type as a string

`vec_ptype_full()` displays the full type of the vector.
`vec_ptype_abbr()` provides an abbreviated summary suitable for use in a
column heading.

## Usage

``` r
vec_ptype_full(x, ...)

vec_ptype_abbr(x, ..., prefix_named = FALSE, suffix_shape = TRUE)
```

## Arguments

- x:

  A vector.

- ...:

  These dots are for future extensions and must be empty.

- prefix_named:

  If `TRUE`, add a prefix for named vectors.

- suffix_shape:

  If `TRUE` (the default), append the shape of the vector.

## Value

A string.

## S3 dispatch

The default method for `vec_ptype_full()` uses the first element of the
class vector. Override this method if your class has parameters that
should be prominently displayed.

The default method for `vec_ptype_abbr()`
[`abbreviate()`](https://rdrr.io/r/base/abbreviate.html)s
`vec_ptype_full()` to 8 characters. You should almost always override,
aiming for 4-6 characters where possible.

These arguments are handled by the generic and not passed to methods:

- `prefix_named`

- `suffix_shape`

## Examples

``` r
cat(vec_ptype_full(1:10))
#> integer
cat(vec_ptype_full(iris))
#> data.frame<
#>   Sepal.Length: double
#>   Sepal.Width : double
#>   Petal.Length: double
#>   Petal.Width : double
#>   Species     : factor<fb977>
#> >

cat(vec_ptype_abbr(1:10))
#> int
```
