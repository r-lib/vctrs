# Coercion between two data frames

`df_ptype2()` and `df_cast()` are the two functions you need to call
from [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)
and [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)
methods for data frame subclasses. See
[?howto-faq-coercion-data-frame](https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.md).
Their main job is to determine the common type of two data frames,
adding and coercing columns as needed, or throwing an incompatible type
error when the columns are not compatible.

## Usage

``` r
df_ptype2(x, y, ..., x_arg = "", y_arg = "", call = caller_env())

df_cast(x, to, ..., x_arg = "", to_arg = "", call = caller_env())

tib_ptype2(x, y, ..., x_arg = "", y_arg = "", call = caller_env())

tib_cast(x, to, ..., x_arg = "", to_arg = "", call = caller_env())
```

## Arguments

- x, y, to:

  Subclasses of data frame.

- ...:

  If you call `df_ptype2()` or `df_cast()` from a
  [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) or
  [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) method,
  you must forward the dots passed to your method on to `df_ptype2()` or
  `df_cast()`.

- x_arg, y_arg:

  Argument names for `x` and `y`. These are used in error messages to
  inform the user about the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)).

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- to_arg:

  Argument name `to` used in error messages to inform the user about the
  locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)).

## Value

- When `x` and `y` are not compatible, an error of class
  `vctrs_error_incompatible_type` is thrown.

- When `x` and `y` are compatible, `df_ptype2()` returns the common type
  as a bare data frame. `tib_ptype2()` returns the common type as a bare
  tibble.
