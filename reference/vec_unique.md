# Find and count unique values

- `vec_unique()`: the unique values. Equivalent to
  [`unique()`](https://rdrr.io/r/base/unique.html).

- `vec_unique_loc()`: the locations of the unique values.

- `vec_unique_count()`: the number of unique values.

## Usage

``` r
vec_unique(x)

vec_unique_loc(x)

vec_unique_count(x)
```

## Arguments

- x:

  A vector (including a data frame).

## Value

- `vec_unique()`: a vector the same type as `x` containing only unique
  values.

- `vec_unique_loc()`: an integer vector, giving locations of unique
  values.

- `vec_unique_count()`: an integer vector of length 1, giving the number
  of unique values.

## Dependencies

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.md)

## Missing values

In most cases, missing values are not considered to be equal, i.e.
`NA == NA` is not `TRUE`. This behaviour would be unappealing here, so
these functions consider all `NAs` to be equal. (Similarly, all `NaN`
are also considered to be equal.)

## See also

[vec_duplicate](https://vctrs.r-lib.org/reference/vec_duplicate.md) for
functions that work with the dual of unique values: duplicated values.

## Examples

``` r
x <- rpois(100, 8)
vec_unique(x)
#>  [1]  4 13 10  5  8  7  9  6 15 12 11 14  2
vec_unique_loc(x)
#>  [1]  1  2  3  4  5  8  9 11 14 20 25 27 31
vec_unique_count(x)
#> [1] 13

# `vec_unique()` returns values in the order that encounters them
# use sort = "location" to match to the result of `vec_count()`
head(vec_unique(x))
#> [1]  4 13 10  5  8  7
head(vec_count(x, sort = "location"))
#>   key count
#> 1   4     3
#> 2  13     4
#> 3  10     6
#> 4   5     9
#> 5   8    15
#> 6   7    17

# Normally missing values are not considered to be equal
NA == NA
#> [1] NA

# But they are for the purposes of considering uniqueness
vec_unique(c(NA, NA, NA, NA, 1, 2, 1))
#> [1] NA  1  2
```
