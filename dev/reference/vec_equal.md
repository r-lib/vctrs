# Equality

`vec_equal()` tests if two vectors are equal.

## Usage

``` r
vec_equal(x, y, na_equal = FALSE, .ptype = NULL)
```

## Arguments

- x, y:

  Vectors with compatible types and lengths.

- na_equal:

  Should `NA` values be considered equal?

- .ptype:

  Override to optionally specify common type

## Value

A logical vector the same size as the common size of `x` and `y`. Will
only contain `NA`s if `na_equal` is `FALSE`.

## Dependencies

- [`vec_cast_common()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md)
  with fallback

- [`vec_recycle_common()`](https://vctrs.r-lib.org/dev/reference/vec_recycle.md)

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md)

## See also

[`vec_detect_missing()`](https://vctrs.r-lib.org/dev/reference/missing.md)

## Examples

``` r
vec_equal(c(TRUE, FALSE, NA), FALSE)
#> [1] FALSE  TRUE    NA
vec_equal(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#> [1] FALSE  TRUE FALSE

vec_equal(5, 1:10)
#>  [1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE
vec_equal("d", letters[1:10])
#>  [1] FALSE FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

df <- data.frame(x = c(1, 1, 2, 1), y = c(1, 2, 1, NA))
vec_equal(df, data.frame(x = 1, y = 2))
#> [1] FALSE  TRUE FALSE    NA
```
