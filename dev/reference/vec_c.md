# Combine many vectors into one vector

Combine all arguments into a new vector of common type.

## Usage

``` r
vec_c(
  ...,
  .ptype = NULL,
  .name_spec = NULL,
  .name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet",
    "universal_quiet"),
  .error_arg = "",
  .error_call = current_env()
)
```

## Arguments

- ...:

  Vectors to coerce.

- .ptype:

  If `NULL`, the default, the output type is determined by computing the
  common type across all elements of `...`.

  Alternatively, you can supply `.ptype` to give the output known type.
  If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this
  value: this is a convenient way to make production code demand fixed
  types.

- .name_spec:

  A name specification for combining inner and outer names. This is
  relevant for inputs passed with a name, when these inputs are
  themselves named, like `outer = c(inner = 1)`, or when they have
  length greater than 1: `outer = 1:2`. By default, these cases trigger
  an error. You can resolve the error by providing a specification that
  describes how to combine the names or the indices of the inner vector
  with the name of the input. This specification can be:

  - A function of two arguments. The outer name is passed as a string to
    the first argument, and the inner names or positions are passed as
    second argument.

  - An anonymous function as a purrr-style formula.

  - A glue specification of the form `"{outer}_{inner}"`.

  - `"inner"`, in which case outer names are ignored, and inner names
    are used if they exist. Note that outer names may still be used to
    provide informative error messages.

  - An [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html)
    object, in which case both outer and inner names are ignored and the
    result is unnamed.

  See the [name specification
  topic](https://vctrs.r-lib.org/dev/reference/name_spec.md).

- .name_repair:

  How to repair names, see `repair` options in
  [`vec_as_names()`](https://vctrs.r-lib.org/dev/reference/vec_as_names.md).

- .error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- .error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A vector with class given by `.ptype`, and length equal to the sum of
the [`vec_size()`](https://vctrs.r-lib.org/dev/reference/vec_size.md) of
the contents of `...`.

The vector will have names if the individual components have names
(inner names) or if the arguments are named (outer names). If both inner
and outer names are present, an error is thrown unless a `.name_spec` is
provided.

## Invariants

- `vec_size(vec_c(x, y)) == vec_size(x) + vec_size(y)`

- `vec_ptype(vec_c(x, y)) == vec_ptype_common(x, y)`.

## Dependencies

### vctrs dependencies

- [`vec_cast_common()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md)
  with fallback

- [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

- [`vec_restore()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

### base dependencies

- [`base::c()`](https://rdrr.io/r/base/c.html)

If inputs inherit from a common class hierarchy, `vec_c()` falls back to
[`base::c()`](https://rdrr.io/r/base/c.html) if there exists a
[`c()`](https://rdrr.io/r/base/c.html) method implemented for this class
hierarchy.

## See also

[`vec_cbind()`](https://vctrs.r-lib.org/dev/reference/vec_bind.md)/[`vec_rbind()`](https://vctrs.r-lib.org/dev/reference/vec_bind.md)
for combining data frames by rows or columns.

## Examples

``` r
vec_c(FALSE, 1L, 1.5)
#> [1] 0.0 1.0 1.5

# Date/times --------------------------
c(Sys.Date(), Sys.time())
#> [1] "2026-01-06" "2026-01-06"
c(Sys.time(), Sys.Date())
#> [1] "2026-01-06 01:10:10 UTC" "2026-01-06 00:00:00 UTC"

vec_c(Sys.Date(), Sys.time())
#> [1] "2026-01-06 00:00:00 UTC" "2026-01-06 01:10:10 UTC"
vec_c(Sys.time(), Sys.Date())
#> [1] "2026-01-06 01:10:10 UTC" "2026-01-06 00:00:00 UTC"

# Factors -----------------------------
c(factor("a"), factor("b"))
#> [1] a b
#> Levels: a b
vec_c(factor("a"), factor("b"))
#> [1] a b
#> Levels: a b


# By default, named inputs must be length 1:
vec_c(name = 1)
#> name 
#>    1 
try(vec_c(name = 1:3))
#> Error in vec_c(name = 1:3) : 
#>   Can't merge the outer name `name` with a vector of length > 1.
#> Please supply a `.name_spec` specification.

# Pass a name specification to work around this:
vec_c(name = 1:3, .name_spec = "{outer}_{inner}")
#> name_1 name_2 name_3 
#>      1      2      3 

# See `?name_spec` for more examples of name specifications.
```
