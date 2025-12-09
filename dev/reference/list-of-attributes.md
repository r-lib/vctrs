# `list_of` attributes

- `list_of_ptype()` returns the `ptype` required by the `list_of`. If no
  `ptype` is required, then `NULL` is returned.

- `list_of_size()` returns the `size` required by the `list_of`. If no
  `size` is required, then `NULL` is returned.

## Usage

``` r
list_of_ptype(x)

list_of_size(x)
```

## Arguments

- x:

  A [list_of](https://vctrs.r-lib.org/dev/reference/list_of.md).

## Examples

``` r
x <- list_of(1, 2)
list_of_ptype(x)
#> numeric(0)
list_of_size(x)
#> NULL

x <- list_of(.ptype = integer(), .size = 5)
list_of_ptype(x)
#> integer(0)
list_of_size(x)
#> [1] 5
```
