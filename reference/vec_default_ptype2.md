# Default cast and ptype2 methods

These functions are automatically called when no
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) or
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) method is
implemented for a pair of types.

- They apply special handling if one of the inputs is of type `AsIs` or
  `sfc`.

- They attempt a number of fallbacks in cases where it would be too
  inconvenient to be strict:

  - If the class and attributes are the same they are considered
    compatible. `vec_default_cast()` returns `x` in this case.

  - In case of incompatible data frame classes, they fall back to
    `data.frame`. If an incompatible subclass of tibble is involved,
    they fall back to `tbl_df`.

- Otherwise, an error is thrown with
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)
  or
  [`stop_incompatible_cast()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md).

## Usage

``` r
vec_default_cast(x, to, ..., x_arg = "", to_arg = "", call = caller_env())

vec_default_ptype2(x, y, ..., x_arg = "", y_arg = "", call = caller_env())
```

## Arguments

- x:

  Vectors to cast.

- to:

  Type to cast to. If `NULL`, `x` will be returned as is.

- ...:

  For
  [`vec_cast_common()`](https://vctrs.r-lib.org/reference/vec_cast.md),
  vectors to cast. For
  [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md),
  `vec_cast_default()`, and
  [`vec_restore()`](https://vctrs.r-lib.org/reference/vec_proxy.md),
  these dots are only for future extensions and should be empty.

- x_arg:

  Argument name for `x`, used in error messages to inform the user about
  the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)).

- to_arg:

  Argument name `to` used in error messages to inform the user about the
  locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
