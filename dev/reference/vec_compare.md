# Compare two vectors

Compare two vectors

## Usage

``` r
vec_compare(x, y, na_equal = FALSE, .ptype = NULL)
```

## Arguments

- x, y:

  Vectors with compatible types and lengths.

- na_equal:

  Should `NA` values be considered equal?

- .ptype:

  Override to optionally specify common type

## Value

An integer vector with values -1 for `x < y`, 0 if `x == y`, and 1 if
`x > y`. If `na_equal` is `FALSE`, the result will be `NA` if either `x`
or `y` is `NA`.

## S3 dispatch

`vec_compare()` is not generic for performance; instead it uses
[`vec_proxy_compare()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_compare.md)
to create a proxy that is used in the comparison.

## Dependencies

- [`vec_cast_common()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md)
  with fallback

- [`vec_recycle_common()`](https://vctrs.r-lib.org/dev/reference/vec_recycle.md)

- [`vec_proxy_compare()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_compare.md)

## Examples

``` r
vec_compare(c(TRUE, FALSE, NA), FALSE)
#> [1]  1  0 NA
vec_compare(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#> [1]  1  0 -1

vec_compare(1:10, 5)
#>  [1] -1 -1 -1 -1  0  1  1  1  1  1
vec_compare(runif(10), 0.5)
#>  [1]  1  1 -1 -1 -1 -1 -1 -1 -1 -1
vec_compare(letters[1:10], "d")
#>  [1] -1 -1 -1  0  1  1  1  1  1  1

df <- data.frame(x = c(1, 1, 1, 2), y = c(0, 1, 2, 1))
vec_compare(df, data.frame(x = 1, y = 1))
#> [1] -1  0  1  1
```
