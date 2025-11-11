# Drop empty elements from a list

`list_drop_empty()` removes empty elements from a list. This includes
`NULL` elements along with empty vectors, like `integer(0)`. This is
equivalent to, but faster than, `vec_slice(x, list_sizes(x) != 0L)`.

## Usage

``` r
list_drop_empty(x)
```

## Arguments

- x:

  A list.

## Dependencies

- [`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)

## Examples

``` r
x <- list(1, NULL, integer(), 2)
list_drop_empty(x)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
```
