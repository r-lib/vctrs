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
#>  [1]  2  1  8  4  9  7  5  6  3 10
vec_sort(x)
#>  [1] 0.274 0.384 0.440 0.449 0.754 0.794 0.810 0.812 0.815    NA
vec_sort(x, direction = "desc")
#>  [1]    NA 0.815 0.812 0.810 0.794 0.754 0.449 0.440 0.384 0.274

# Can also handle data frames
df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
vec_order(df)
#>  [1]  2  1  4  7  3  8  9  5  6 10
vec_sort(df)
#>    g     x
#> 1  1 0.274
#> 2  1 0.384
#> 3  1 0.449
#> 4  1 0.794
#> 5  1 0.815
#> 6  2 0.440
#> 7  2 0.754
#> 8  2 0.810
#> 9  2 0.812
#> 10 2    NA
vec_sort(df, direction = "desc")
#>    g     x
#> 1  2    NA
#> 2  2 0.812
#> 3  2 0.810
#> 4  2 0.754
#> 5  2 0.440
#> 6  1 0.815
#> 7  1 0.794
#> 8  1 0.449
#> 9  1 0.384
#> 10 1 0.274

# Missing values interpreted as largest values are last when
# in increasing order:
vec_order(c(1, NA), na_value = "largest", direction = "asc")
#> [1] 1 2
vec_order(c(1, NA), na_value = "largest", direction = "desc")
#> [1] 2 1
```
