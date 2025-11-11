# A 1d vector of unspecified type

This is a [partial
type](https://vctrs.r-lib.org/dev/reference/new_partial.md) used to
represent logical vectors that only contain `NA`. These require special
handling because we want to allow `NA` to specify missingness without
requiring a type.

## Usage

``` r
unspecified(n = 0)
```

## Arguments

- n:

  Length of vector

## Examples

``` r
vec_ptype_show()
#> Prototype: NULL
vec_ptype_show(NA)
#> Prototype: logical

vec_c(NA, factor("x"))
#> [1] <NA> x   
#> Levels: x
vec_c(NA, Sys.Date())
#> [1] NA           "2025-11-11"
vec_c(NA, Sys.time())
#> [1] NA                        "2025-11-11 19:11:06 UTC"
vec_c(NA, list(1:3, 4:5))
#> [[1]]
#> NULL
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 4 5
#> 
```
