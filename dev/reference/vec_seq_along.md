# Useful sequences

`vec_seq_along()` is equivalent to
[`seq_along()`](https://rdrr.io/r/base/seq.html) but uses size, not
length. `vec_init_along()` creates a vector of missing values with size
matching an existing object.

## Usage

``` r
vec_seq_along(x)

vec_init_along(x, y = x)
```

## Arguments

- x, y:

  Vectors

## Value

- `vec_seq_along()` an integer vector with the same size as `x`.

- `vec_init_along()` a vector with the same type as `x` and the same
  size as `y`.

## Examples

``` r
vec_seq_along(mtcars)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
#> [23] 23 24 25 26 27 28 29 30 31 32
vec_init_along(head(mtcars))
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> ...1  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
#> ...2  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
#> ...3  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
#> ...4  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
#> ...5  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
#> ...6  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
```
