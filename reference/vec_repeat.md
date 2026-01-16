# Expand the length of a vector

**\[deprecated\]**

`vec_repeat()` has been replaced with
[`vec_rep()`](https://vctrs.r-lib.org/reference/vec-rep.md) and
[`vec_rep_each()`](https://vctrs.r-lib.org/reference/vec-rep.md) and is
deprecated as of vctrs 0.3.0.

## Usage

``` r
vec_repeat(x, each = 1L, times = 1L)
```

## Arguments

- x:

  A vector.

- each:

  Number of times to repeat each element of `x`.

- times:

  Number of times to repeat the whole vector of `x`.

## Value

A vector the same type as `x` with size `vec_size(x) * times * each`.
