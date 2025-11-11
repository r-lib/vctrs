# Combine a list of vectors

`list_combine()` is a more powerful version of
[`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md). While
[`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md) is used for
sequential combination, `list_combine()` takes a list of `indices` that
specify where to place each element in the output.

If you have a list of vectors and just need to combine them
sequentially, you'll still want to use `vec_c(!!!x)`.

## Usage

``` r
list_combine(
  x,
  ...,
  indices,
  size,
  default = NULL,
  unmatched = "default",
  multiple = "last",
  slice_x = FALSE,
  ptype = NULL,
  name_spec = NULL,
  name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet",
    "universal_quiet"),
  x_arg = "x",
  indices_arg = "indices",
  default_arg = "default",
  error_call = current_env()
)
```

## Arguments

- x:

  A list of vectors.

  If `slice_x = FALSE`, each element must be size 1 or the same size as
  its corresponding index in `indices` after that index has been
  converted to a positive integer location vector with
  [`vec_as_location()`](https://vctrs.r-lib.org/dev/reference/vec_as_location.md).

  If `slice_x = TRUE`, each element must be size 1 or size `size`.

- ...:

  These dots are for future extensions and must be empty.

- indices:

  A list of indices.

  Indices can be provided in one of two forms:

  - Positive integer vectors of locations less than or equal to `size`.
    Each vector can be any size.

  - Logical vectors of size `size` where `TRUE` denotes the location in
    the output to assign to, and the location from the `x` element to
    pull from. Both `NA` and `FALSE` are considered unmatched.

  The size of `indices` must match the size of `x`.

- size:

  The output size.

- default:

  If `NULL`, a missing value is used for locations unmatched by
  `indices`, otherwise the provided `default` is used.

  If provided, `default` must be size 1 or size `size`.

  Can only be set when `unmatched = "default"`.

- unmatched:

  Handling of locations in the output unmatched by `indices`. One of:

  - `"default"` to use `default` in unmatched locations.

  - `"error"` to error when there are unmatched locations.

- multiple:

  Handling of locations in the output matched by multiple `indices`.

  - `"last"` uses the value from the last matched index.

  - `"first"` uses the value from the first matched index.

  Note that `multiple` only applies across `indices`. Within a single
  index if there are overlapping locations, then the last will always
  win. This can only occur with integer `indices`, as you can't overlap
  within an index when using logical `indices`.

- slice_x:

  A boolean.

  If `TRUE`, each element of `x` is sliced by its corresponding index
  from `indices` before being assigned into the output, which is
  effectively the same as
  `map2(list(x, indices), function(x, index) vec_slice(x, index))`, but
  is optimized to avoid materializing the slices.

  See the `slice_value` argument of
  [`vec_assign()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)
  for more examples.

- ptype:

  If `NULL`, the output type is determined by computing the common type
  across all elements of `x` and `default`. Alternatively, you can
  supply `ptype` to give the output a known type.

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

- x_arg, indices_arg, default_arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A vector of type `vec_ptype_common(!!!x)`, or `ptype`, if specified.

The size of the output is determined by `size`.

## Examples

``` r
# Combine a list of vectors using
# a list of `indices`
x <- list(
  1:3,
  4:6,
  7:8
)
indices <- list(
  c(1, 3, 7),
  c(8, 6, 5),
  c(2, 4)
)
list_combine(x, indices = indices, size = 8)
#> [1] 1 7 2 8 6 5 3 4

# Overlapping `indices` are allowed.
# The last match "wins" by default.
x <- list(
  1:3,
  4:6
)
indices <- list(
  c(1, 2, 3),
  c(1, 2, 6)
)
list_combine(x, indices = indices, size = 6)
#> [1]  4  5  3 NA NA  6

# Use `multiple` to force the first match to win.
# This is similar to how `dplyr::case_when()` works.
list_combine(x, indices = indices, size = 6, multiple = "first")
#> [1]  1  2  3 NA NA  6

# Works with data frames as well.
# Now how index 2 is not assigned to.
x <- list(
  data.frame(x = 1:2, y = c("a", "b")),
  data.frame(x = 3:4, y = c("c", "d"))
)
indices <- list(
  c(4, 1),
  c(3, NA)
)
list_combine(x, indices = indices, size = 4)
#>    x    y
#> 1  2    b
#> 2 NA <NA>
#> 3  3    c
#> 4  1    a

# You can use `size` to combine into a larger object than you have values for
list_combine(list(1:2, 4:5), indices = list(1:2, 4:5), size = 8)
#> [1]  1  2 NA  4  5 NA NA NA

# Additionally specifying `default` allows you to control the value used in
# unfilled locations
list_combine(
  list(1:2, 4:5),
  indices = list(1:2, 4:5),
  size = 8,
  default = 0L
)
#> [1] 1 2 0 4 5 0 0 0

# Alternatively, if you'd like to assert that you've covered all output
# locations through `indices`, set `unmatched = "error"`.
# Here, we've set the size to 5 but missed location 3:
try(list_combine(
  list(1:2, 4:5),
  indices = list(1:2, 4:5),
  size = 5,
  unmatched = "error"
))
#> Error in list_combine(list(1:2, 4:5), indices = list(1:2, 4:5), size = 5,  : 
#>   Each location must be matched.
#> ✖ Location 3 is unmatched.
```
