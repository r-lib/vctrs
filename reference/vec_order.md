# Order and sort vectors

Order and sort vectors

## Usage

``` r
vec_order(
  x,
  ...,
  direction = c("asc", "desc"),
  na_value = c("largest", "smallest")
)

vec_sort(
  x,
  ...,
  direction = c("asc", "desc"),
  na_value = c("largest", "smallest")
)
```

## Arguments

- x:

  A vector

- ...:

  These dots are for future extensions and must be empty.

- direction:

  Direction to sort in. Defaults to `asc`ending.

- na_value:

  Should `NA`s be treated as the largest or smallest values?

## Value

- `vec_order()` an integer vector the same size as `x`.

- `vec_sort()` a vector with the same size and type as `x`.

## Differences with [`order()`](https://rdrr.io/r/base/order.html)

Unlike the `na.last` argument of
[`order()`](https://rdrr.io/r/base/order.html) which decides the
positions of missing values irrespective of the `decreasing` argument,
the `na_value` argument of `vec_order()` interacts with `direction`. If
missing values are considered the largest value, they will appear last
in ascending order, and first in descending order.

## Dependencies of `vec_order()`

- [`vec_proxy_order()`](https://vctrs.r-lib.org/reference/vec_proxy_compare.md)

## Dependencies of `vec_sort()`

- [`vec_proxy_order()`](https://vctrs.r-lib.org/reference/vec_proxy_compare.md)

- `vec_order()`

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

## Examples

``` r
x <- round(c(runif(9), NA), 3)
vec_order(x)
#>  [1]  3  6  5  9  1  4  7  2  8 10
vec_sort(x)
#>  [1] 0.122 0.128 0.207 0.374 0.442 0.561 0.753 0.799 0.895    NA
vec_sort(x, direction = "desc")
#>  [1]    NA 0.895 0.799 0.753 0.561 0.442 0.374 0.207 0.128 0.122

# Can also handle data frames
df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
vec_order(df)
#>  [1]  5  9  1  4  7  8 10  3  6  2
vec_sort(df)
#>    g     x
#> 1  1 0.207
#> 2  1 0.374
#> 3  1 0.442
#> 4  1 0.561
#> 5  1 0.753
#> 6  1 0.895
#> 7  1    NA
#> 8  2 0.122
#> 9  2 0.128
#> 10 2 0.799
vec_sort(df, direction = "desc")
#>    g     x
#> 1  2 0.799
#> 2  2 0.128
#> 3  2 0.122
#> 4  1    NA
#> 5  1 0.895
#> 6  1 0.753
#> 7  1 0.561
#> 8  1 0.442
#> 9  1 0.374
#> 10 1 0.207

# Missing values interpreted as largest values are last when
# in increasing order:
vec_order(c(1, NA), na_value = "largest", direction = "asc")
#> [1] 1 2
vec_order(c(1, NA), na_value = "largest", direction = "desc")
#> [1] 2 1
```
