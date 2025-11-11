# Fill in missing values with the previous or following value

`vec_fill_missing()` fills gaps of missing values with the previous or
following non-missing value.

## Usage

``` r
vec_fill_missing(
  x,
  direction = c("down", "up", "downup", "updown"),
  max_fill = NULL
)
```

## Arguments

- x:

  A vector

- direction:

  Direction in which to fill missing values. Must be either `"down"`,
  `"up"`, `"downup"`, or `"updown"`.

- max_fill:

  A single positive integer specifying the maximum number of sequential
  missing values that will be filled. If `NULL`, there is no limit.

## Examples

``` r
x <- c(NA, NA, 1, NA, NA, NA, 3, NA, NA)

# Filling down replaces missing values with the previous non-missing value
vec_fill_missing(x, direction = "down")
#> [1] NA NA  1  1  1  1  3  3  3

# To also fill leading missing values, use `"downup"`
vec_fill_missing(x, direction = "downup")
#> [1] 1 1 1 1 1 1 3 3 3

# Limit the number of sequential missing values to fill with `max_fill`
vec_fill_missing(x, max_fill = 1)
#> [1] NA NA  1  1 NA NA  3  3 NA

# Data frames are filled rowwise. Rows are only considered missing
# if all elements of that row are missing.
y <- c(1, NA, 2, NA, NA, 3, 4, NA, 5)
df <- data_frame(x = x, y = y)
df
#> # A tibble: 9 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1    NA     1
#> 2    NA    NA
#> 3     1     2
#> 4    NA    NA
#> 5    NA    NA
#> 6    NA     3
#> 7     3     4
#> 8    NA    NA
#> 9    NA     5

vec_fill_missing(df)
#> # A tibble: 9 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1    NA     1
#> 2    NA     1
#> 3     1     2
#> 4     1     2
#> 5     1     2
#> 6    NA     3
#> 7     3     4
#> 8     3     4
#> 9    NA     5
```
