# Combine a list of vectors

While `list_unchop()` is not deprecated, we now recommend that you use
either:

- `list_combine(x, indices = indices, size = size)` over
  `list_unchop(x, indices = indices)`

- `vec_c(!!!x)` over `list_unchop(x)`

`list_unchop()` combines a list of vectors into a single vector, placing
elements in the output according to the locations specified by
`indices`. It is similar to
[`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md), but gives
greater control over how the elements are combined.

## Usage

``` r
list_unchop(
  x,
  ...,
  indices = NULL,
  ptype = NULL,
  name_spec = NULL,
  name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet",
    "universal_quiet"),
  error_arg = "x",
  error_call = current_env()
)
```

## Arguments

- x:

  A list

- ...:

  These dots are for future extensions and must be empty.

- indices:

  A list of positive integer vectors specifying the locations to place
  elements of `x` in. Each element of `x` is recycled to the size of the
  corresponding index vector. The size of `indices` must match the size
  of `x`. If `NULL`, `x` is combined in the order it is provided in,
  which is equivalent to using
  [`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md).

- ptype:

  If `NULL`, the default, the output type is determined by computing the
  common type across all elements of `x`. Alternatively, you can supply
  `ptype` to give the output a known type.

- name_spec:

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

- name_repair:

  How to repair names, see `repair` options in
  [`vec_as_names()`](https://vctrs.r-lib.org/dev/reference/vec_as_names.md).

- error_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if specified. The
size is computed as `vec_size_common(!!!indices)` unless the indices are
`NULL`, in which case the size is `vec_size_common(!!!x)`.

## Dependencies

- [`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md)

## Examples

``` r
# If `indices` selects every value in `x` exactly once,
# in any order, then `list_unchop()` inverts `vec_chop()`
x <- c("a", "b", "c", "d")
indices <- list(2, c(3, 1), 4)
vec_chop(x, indices = indices)
#> [[1]]
#> [1] "b"
#> 
#> [[2]]
#> [1] "c" "a"
#> 
#> [[3]]
#> [1] "d"
#> 
list_unchop(vec_chop(x, indices = indices), indices = indices)
#> [1] "a" "b" "c" "d"

# When unchopping, size 1 elements of `x` are recycled
# to the size of the corresponding index
list_unchop(list(1, 2:3), indices = list(c(1, 3, 5), c(2, 4)))
#> [1] 1 2 1 3 1

# Names are retained, and outer names can be combined with inner
# names through the use of a `name_spec`
lst <- list(x = c(a = 1, b = 2), y = 1)
list_unchop(lst, indices = list(c(3, 2), c(1, 4)), name_spec = "{outer}_{inner}")
#> y_1 x_b x_a y_2 
#>   1   2   1   1 

# If you have a list of homogeneous vectors, sometimes it can be useful to
# unchop, apply a function to the flattened vector, and then rechop according
# to the original indices. This can be done efficiently with `list_sizes()`.
x <- list(c(1, 2, 1), c(3, 1), 5, double())
x_flat <- list_unchop(x)
x_flat <- x_flat + max(x_flat)
vec_chop(x_flat, sizes = list_sizes(x))
#> [[1]]
#> [1] 6 7 6
#> 
#> [[2]]
#> [1] 8 6
#> 
#> [[3]]
#> [1] 10
#> 
#> [[4]]
#> numeric(0)
#> 
```
