# Lossy cast error

**\[experimental\]**

By default, lossy casts are an error. Use
[`allow_lossy_cast()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)
to silence these errors and continue with the partial results. In this
case the lost values are typically set to `NA` or to a lower value
resolution, depending on the type of cast.

Lossy cast errors are thrown by `maybe_lossy_cast()`. Unlike functions
prefixed with `stop_`, `maybe_lossy_cast()` usually returns a result. If
a lossy cast is detected, it throws an error, unless it's been wrapped
in
[`allow_lossy_cast()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md).
In that case, it returns the result silently.

## Usage

``` r
maybe_lossy_cast(
  result,
  x,
  to,
  lossy = NULL,
  locations = NULL,
  ...,
  loss_type = c("precision", "generality"),
  x_arg,
  to_arg,
  call = caller_env(),
  details = NULL,
  message = NULL,
  class = NULL,
  .deprecation = FALSE
)
```

## Arguments

- result:

  The result of a potentially lossy cast.

- x:

  Vectors to cast.

- to:

  Type to cast to.

- lossy:

  A logical vector indicating which elements of `result` were lossy.

  Can also be a single `TRUE`, but note that `locations` picks up
  locations from this vector by default. In this case, supply your own
  location vector, possibly empty.

- locations:

  An optional integer vector giving the locations where `x` lost
  information.

- ..., class:

  Only use these fields when creating a subclass.

- loss_type:

  The kind of lossy cast to be mentioned in error messages. Can be loss
  of precision (for instance from double to integer) or loss of
  generality (from character to factor).

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

- details:

  Any additional human readable details.

- message:

  An overriding message for the error. `details` and `message` are
  mutually exclusive, supplying both is an error.

- .deprecation:

  If `TRUE`, the error is downgraded to a deprecation warning. This is
  useful for transitioning your class to a stricter conversion scheme.
  The warning advises your users to wrap their code with
  [`allow_lossy_cast()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md).
