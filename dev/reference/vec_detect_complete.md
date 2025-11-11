# Complete

`vec_detect_complete()` detects "complete" observations. An observation
is considered complete if it is non-missing. For most vectors, this
implies that `vec_detect_complete(x) == !vec_detect_missing(x)`.

For data frames and matrices, a row is only considered complete if all
elements of that row are non-missing. To compare,
`!vec_detect_missing(x)` detects rows that are partially complete (they
have at least one non-missing value).

## Usage

``` r
vec_detect_complete(x)
```

## Arguments

- x:

  A vector

## Value

A logical vector with the same size as `x`.

## Details

A [record](https://vctrs.r-lib.org/dev/reference/new_rcrd.md) type
vector is similar to a data frame, and is only considered complete if
all fields are non-missing.

## See also

[`stats::complete.cases()`](https://rdrr.io/r/stats/complete.cases.html)

## Examples

``` r
x <- c(1, 2, NA, 4, NA)

# For most vectors, this is identical to `!vec_detect_missing(x)`
vec_detect_complete(x)
#> [1]  TRUE  TRUE FALSE  TRUE FALSE
!vec_detect_missing(x)
#> [1]  TRUE  TRUE FALSE  TRUE FALSE

df <- data_frame(
  x = x,
  y = c("a", "b", NA, "d", "e")
)

# This returns `TRUE` where all elements of the row are non-missing.
# Compare that with `!vec_detect_missing()`, which detects rows that have at
# least one non-missing value.
df2 <- df
df2$all_non_missing <- vec_detect_complete(df)
df2$any_non_missing <- !vec_detect_missing(df)
df2
#> # A tibble: 5 × 4
#>       x y     all_non_missing any_non_missing
#>   <dbl> <chr> <lgl>           <lgl>          
#> 1     1 a     TRUE            TRUE           
#> 2     2 b     TRUE            TRUE           
#> 3    NA NA    FALSE           FALSE          
#> 4     4 d     TRUE            TRUE           
#> 5    NA e     FALSE           TRUE           
```
