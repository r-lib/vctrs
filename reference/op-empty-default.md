# Default value for empty vectors

Use this inline operator when you need to provide a default value for
empty (as defined by
[`vec_is_empty()`](https://vctrs.r-lib.org/reference/vec_size.md))
vectors.

## Usage

``` r
x %0% y
```

## Arguments

- x:

  A vector

- y:

  Value to use if `x` is empty. To preserve type-stability, should be
  the same type as `x`.

## Examples

``` r
1:10 %0% 5
#>  [1]  1  2  3  4  5  6  7  8  9 10
integer() %0% 5
#> [1] 5
```
