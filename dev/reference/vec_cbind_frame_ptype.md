# Frame prototype

**\[experimental\]**

This is an experimental generic that returns zero-columns variants of a
data frame. It is needed for
[`vec_cbind()`](https://vctrs.r-lib.org/dev/reference/vec_bind.md), to
work around the lack of colwise primitives in vctrs. Expect changes.

## Usage

``` r
vec_cbind_frame_ptype(x, ...)
```

## Arguments

- x:

  A data frame.

- ...:

  These dots are for future extensions and must be empty.
