# Custom conditions for vctrs package

These functions are called for their side effect of raising errors and
warnings. These conditions have custom classes and structures to make
testing easier.

## Usage

``` r
stop_incompatible_type(
  x,
  y,
  ...,
  x_arg,
  y_arg,
  action = c("combine", "convert"),
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
)

stop_incompatible_cast(
  x,
  to,
  ...,
  x_arg,
  to_arg,
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
)

stop_incompatible_op(
  op,
  x,
  y,
  details = NULL,
  ...,
  message = NULL,
  class = NULL,
  call = caller_env()
)

stop_incompatible_size(
  x,
  y,
  x_size,
  y_size,
  ...,
  x_arg,
  y_arg,
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
)

allow_lossy_cast(expr, x_ptype = NULL, to_ptype = NULL)
```

## Arguments

- x, y, to:

  Vectors

- ..., class:

  Only use these fields when creating a subclass.

- x_arg, y_arg, to_arg:

  Argument names for `x`, `y`, and `to`. Used in error messages to
  inform the user about the locations of incompatible types.

- action:

  An option to customize the incompatible type message depending on the
  context. Errors thrown from
  [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
  use `"combine"` and those thrown from
  [`vec_cast()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md) use
  `"convert"`.

- details:

  Any additional human readable details.

- message:

  An overriding message for the error. `details` and `message` are
  mutually exclusive, supplying both is an error.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- x_ptype, to_ptype:

  Suppress only the casting errors where `x` or `to` match these
  [prototypes](https://vctrs.r-lib.org/dev/reference/vec_ptype.md).

## Value

`stop_incompatible_*()` unconditionally raise an error of class
`"vctrs_error_incompatible_*"` and `"vctrs_error_incompatible"`.

## Examples

``` r
# Most of the time, `maybe_lossy_cast()` returns its input normally:
maybe_lossy_cast(
  c("foo", "bar"),
  NA,
  "",
  lossy = c(FALSE, FALSE),
  x_arg = "",
  to_arg = ""
)
#> [1] "foo" "bar"

# If `lossy` has any `TRUE`, an error is thrown:
try(maybe_lossy_cast(
  c("foo", "bar"),
  NA,
  "",
  lossy = c(FALSE, TRUE),
  x_arg = "",
  to_arg = ""
))
#> Error in eval(expr, envir) : 
#>   Can't convert from <logical> to <character> due to loss of precision.
#> • Locations: 2

# Unless lossy casts are allowed:
allow_lossy_cast(
  maybe_lossy_cast(
    c("foo", "bar"),
    NA,
    "",
    lossy = c(FALSE, TRUE),
    x_arg = "",
    to_arg = ""
  )
)
#> [1] "foo" "bar"
```
