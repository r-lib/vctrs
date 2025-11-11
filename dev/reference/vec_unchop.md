# Chopping

**\[deprecated\]**

`vec_unchop()` has been renamed to
[`list_unchop()`](https://vctrs.r-lib.org/dev/reference/list_unchop.md)
and is deprecated as of vctrs 0.5.0.

## Usage

``` r
vec_unchop(
  x,
  indices = NULL,
  ptype = NULL,
  name_spec = NULL,
  name_repair = c("minimal", "unique", "check_unique", "universal")
)
```

## Arguments

- x:

  A list

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

## Value

A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if specified. The
size is computed as `vec_size_common(!!!indices)` unless the indices are
`NULL`, in which case the size is `vec_size_common(!!!x)`.
