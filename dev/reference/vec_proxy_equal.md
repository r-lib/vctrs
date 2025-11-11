# Equality proxy

Returns a proxy object (i.e. an atomic vector or data frame of atomic
vectors). For
[vctr](https://vctrs.r-lib.org/dev/reference/new_vctr.md)s, this
determines the behaviour of `==` and `!=` (via
[`vec_equal()`](https://vctrs.r-lib.org/dev/reference/vec_equal.md));
[`unique()`](https://rdrr.io/r/base/unique.html),
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) (via
[`vec_unique()`](https://vctrs.r-lib.org/dev/reference/vec_unique.md)
and
[`vec_duplicate_detect()`](https://vctrs.r-lib.org/dev/reference/vec_duplicate.md));
[`is.na()`](https://rdrr.io/r/base/NA.html) and
[`anyNA()`](https://rdrr.io/r/base/NA.html) (via
[`vec_detect_missing()`](https://vctrs.r-lib.org/dev/reference/missing.md)).

## Usage

``` r
vec_proxy_equal(x, ...)
```

## Arguments

- x:

  A vector x.

- ...:

  These dots are for future extensions and must be empty.

## Value

A 1d atomic vector or a data frame.

## Details

The default method calls
[`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md), as
the default underlying vector data should be equal-able in most cases.
If your class is not equal-able, provide a `vec_proxy_equal()` method
that throws an error.

## Data frames

If the proxy for `x` is a data frame, the proxy function is
automatically recursively applied on all columns as well. After applying
the proxy recursively, if there are any data frame columns present in
the proxy, then they are unpacked. Finally, if the resulting data frame
only has a single column, then it is unwrapped and a vector is returned
as the proxy.

## Dependencies

- [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)
  called by default
