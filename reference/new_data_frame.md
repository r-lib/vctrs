# Assemble attributes for data frame construction

`new_data_frame()` constructs a new data frame from an existing list. It
is meant to be performant, and does not check the inputs for correctness
in any way. It is only safe to use after a call to
[`df_list()`](https://vctrs.r-lib.org/reference/df_list.md), which
collects and validates the columns used to construct the data frame.

## Usage

``` r
new_data_frame(x = list(), n = NULL, ..., class = NULL)
```

## Arguments

- x:

  A named list of equal-length vectors. The lengths are not checked; it
  is responsibility of the caller to make sure they are equal.

- n:

  Number of rows. If `NULL`, will be computed from the length of the
  first element of `x`.

- ..., class:

  Additional arguments for creating subclasses.

  The following attributes have special behavior:

  - `"names"` is preferred if provided, overriding existing names in
    `x`.

  - `"row.names"` is preferred if provided, overriding both `n` and the
    size implied by `x`.

## See also

[`df_list()`](https://vctrs.r-lib.org/reference/df_list.md) for a way to
safely construct a data frame's underlying data structure from
individual columns. This can be used to create a named list for further
use by `new_data_frame()`.

## Examples

``` r
new_data_frame(list(x = 1:10, y = 10:1))
#>     x  y
#> 1   1 10
#> 2   2  9
#> 3   3  8
#> 4   4  7
#> 5   5  6
#> 6   6  5
#> 7   7  4
#> 8   8  3
#> 9   9  2
#> 10 10  1
```
