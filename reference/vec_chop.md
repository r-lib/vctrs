# Chopping

`vec_chop()` provides an efficient method to repeatedly slice a vector.
It captures the pattern of `map(indices, vec_slice, x = x)`. When no
indices are supplied, it is generally equivalent to
[`as.list()`](https://rdrr.io/r/base/list.html).

## Usage

``` r
vec_chop(x, ..., indices = NULL, sizes = NULL)
```

## Arguments

- x:

  A vector

- ...:

  These dots are for future extensions and must be empty.

- indices:

  A list of positive integer vectors to slice `x` with, or `NULL`. Can't
  be used if `sizes` is already specified. If both `indices` and `sizes`
  are `NULL`, `x` is split into its individual elements, equivalent to
  using an `indices` of `as.list(vec_seq_along(x))`.

- sizes:

  An integer vector of non-negative sizes representing sequential
  indices to slice `x` with, or `NULL`. Can't be used if `indices` is
  already specified.

  For example, `sizes = c(2, 4)` is equivalent to
  `indices = list(1:2, 3:6)`, but is typically faster.

  `sum(sizes)` must be equal to `vec_size(x)`, i.e. `sizes` must
  completely partition `x`, but an individual size is allowed to be `0`.

## Value

A list where each element has the same type as `x`. The size of the list
is equal to `vec_size(indices)`, `vec_size(sizes)`, or `vec_size(x)`
depending on whether or not `indices` or `sizes` is provided.

## Dependencies

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

## Examples

``` r
vec_chop(1:5)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 

# These two are equivalent
vec_chop(1:5, indices = list(1:2, 3:5))
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 3 4 5
#> 
vec_chop(1:5, sizes = c(2, 3))
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 3 4 5
#> 

# Can also be used on data frames
vec_chop(mtcars, indices = list(1:3, 4:6))
#> [[1]]
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 
#> [[2]]
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#> 

# If you know your input is sorted and you'd like to split on the groups,
# `vec_run_sizes()` can be efficiently combined with `sizes`
df <- data_frame(
  g = c(2, 5, 5, 6, 6, 6, 6, 8, 9, 9),
  x = 1:10
)
vec_chop(df, sizes = vec_run_sizes(df$g))
#> [[1]]
#> # A tibble: 1 × 2
#>       g     x
#>   <dbl> <int>
#> 1     2     1
#> 
#> [[2]]
#> # A tibble: 2 × 2
#>       g     x
#>   <dbl> <int>
#> 1     5     2
#> 2     5     3
#> 
#> [[3]]
#> # A tibble: 4 × 2
#>       g     x
#>   <dbl> <int>
#> 1     6     4
#> 2     6     5
#> 3     6     6
#> 4     6     7
#> 
#> [[4]]
#> # A tibble: 1 × 2
#>       g     x
#>   <dbl> <int>
#> 1     8     8
#> 
#> [[5]]
#> # A tibble: 2 × 2
#>       g     x
#>   <dbl> <int>
#> 1     9     9
#> 2     9    10
#> 

# If you have a list of homogeneous vectors, sometimes it can be useful to
# combine, apply a function to the flattened vector, and chop according
# to the original indices. This can be done efficiently with `list_sizes()`.
x <- list(c(1, 2, 1), c(3, 1), 5, double())
x_flat <- vec_c(!!!x)
x_flat <- x_flat + max(x_flat)
vec_chop(x_flat, sizes = list_sizes(x))
#> [[1]]
#> [1] 6 7 6
#> 
#> [[2]]
#> [1] 8 6
#> 
#> [[3]]
#> [1] 10
#> 
#> [[4]]
#> numeric(0)
#> 
```
