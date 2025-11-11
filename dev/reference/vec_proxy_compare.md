# Comparison and order proxy

`vec_proxy_compare()` and `vec_proxy_order()` return proxy objects, i.e.
an atomic vector or data frame of atomic vectors.

For [`vctrs_vctr`](https://vctrs.r-lib.org/dev/reference/new_vctr.md)
objects:

- `vec_proxy_compare()` determines the behavior of `<`, `>`, `>=` and
  `<=` (via
  [`vec_compare()`](https://vctrs.r-lib.org/dev/reference/vec_compare.md));
  and [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),
  [`median()`](https://rdrr.io/r/stats/median.html), and
  [`quantile()`](https://rdrr.io/r/stats/quantile.html).

- `vec_proxy_order()` determines the behavior of
  [`order()`](https://rdrr.io/r/base/order.html) and
  [`sort()`](https://rdrr.io/r/base/sort.html) (via
  [`xtfrm()`](https://rdrr.io/r/base/xtfrm.html)).

## Usage

``` r
vec_proxy_compare(x, ...)

vec_proxy_order(x, ...)
```

## Arguments

- x:

  A vector x.

- ...:

  These dots are for future extensions and must be empty.

## Value

A 1d atomic vector or a data frame.

## Details

The default method of `vec_proxy_compare()` assumes that all classes
built on top of atomic vectors or records are comparable. Internally the
default calls
[`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md).
If your class is not comparable, you will need to provide a
`vec_proxy_compare()` method that throws an error.

The behavior of `vec_proxy_order()` is identical to
`vec_proxy_compare()`, with the exception of lists. Lists are not
comparable, as comparing elements of different types is undefined.
However, to allow ordering of data frames containing list-columns, the
ordering proxy of a list is generated as an integer vector that can be
used to order list elements by first appearance.

If a class implements a `vec_proxy_compare()` method, it usually doesn't
need to provide a `vec_proxy_order()` method, because the latter is
implemented by forwarding to `vec_proxy_compare()` by default. Classes
inheriting from list are an exception: due to the default
`vec_proxy_order()` implementation, `vec_proxy_compare()` and
`vec_proxy_order()` should be provided for such classes (with identical
implementations) to avoid mismatches between comparison and sorting.

## Dependencies

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md)
  called by default in `vec_proxy_compare()`

- `vec_proxy_compare()` called by default in `vec_proxy_order()`

## Data frames

If the proxy for `x` is a data frame, the proxy function is
automatically recursively applied on all columns as well. After applying
the proxy recursively, if there are any data frame columns present in
the proxy, then they are unpacked. Finally, if the resulting data frame
only has a single column, then it is unwrapped and a vector is returned
as the proxy.

## Examples

``` r
# Lists are not comparable
x <- list(1:2, 1, 1:2, 3)
try(vec_compare(x, x))
#> Error in vec_proxy_compare(x = x) : 
#>   `vec_proxy_compare.list()` not supported.

# But lists are orderable by first appearance to allow for
# ordering data frames with list-cols
df <- new_data_frame(list(x = x))
vec_sort(df)
#>      x
#> 1 1, 2
#> 2 1, 2
#> 3    1
#> 4    3
```
