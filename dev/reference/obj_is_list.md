# List checks

- `obj_is_list()` tests if `x` is considered a list in the vctrs sense.
  It returns `TRUE` if all of the following hold:

  - `x` must have list storage, i.e. `typeof(x)` returns `"list"`

  - `x` must not have a `dim` attribute

  - `x` must not have a `class` attribute, or must explicitly inherit
    from `"list"` as the last class

- `list_all_vectors()` takes a list and returns `TRUE` if all elements
  of that list are vectors.

- `list_all_size()` takes a list and returns `TRUE` if all elements of
  that list have the same `size`.

- `list_all_recyclable()` takes a list and returns `TRUE` if all
  elements of that list can recycle to `size`.

- `obj_check_list()`, `list_check_all_vectors()`,
  `list_check_all_size()`, and `list_check_all_recyclable()` use the
  above functions, but throw a standardized and informative error if
  they return `FALSE`.

## Usage

``` r
obj_is_list(x)

obj_check_list(x, ..., arg = caller_arg(x), call = caller_env())

list_all_vectors(x, ..., allow_null = FALSE)

list_check_all_vectors(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

list_all_size(x, size, ..., allow_null = FALSE)

list_check_all_size(
  x,
  size,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)

list_all_recyclable(x, size, ..., allow_null = FALSE)

list_check_all_recyclable(
  x,
  size,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  For `vec_*()` functions, an object. For `list_*()` functions, a list.

- ...:

  These dots are for future extensions and must be empty.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- allow_null:

  Whether `NULL` elements should be skipped over automatically or not.

- size:

  The size to check each element for compatibility with.

## Details

Notably, data frames and S3 record style classes like POSIXlt are not
considered lists.

## See also

[`list_sizes()`](https://vctrs.r-lib.org/dev/reference/vec_size.md)

## Examples

``` r
obj_is_list(list())
#> [1] TRUE
obj_is_list(list_of(1))
#> [1] TRUE
obj_is_list(data.frame())
#> [1] FALSE

list_all_vectors(list(1, mtcars))
#> [1] TRUE
list_all_vectors(list(1, environment()))
#> [1] FALSE

list_all_size(list(1:2, 2:3), 2)
#> [1] TRUE
list_all_size(list(1:2, 2:4), 2)
#> [1] FALSE

list_all_recyclable(list(1, 2:3), 2)
#> [1] TRUE
list_all_recyclable(list(1, 2:4), 2)
#> [1] FALSE

# `list_`-prefixed functions assume a list:
try(list_all_vectors(environment()))
#> Error in list_all_vectors(environment()) : 
#>   `x` must be a list, not an environment.

# `NULL` elements are not considered vectors and generally have a size of 0
try(list_check_all_vectors(list(1, NULL, 2)))
#> Error in eval(expr, envir) : 
#>   `list(1, NULL, 2)[[2]]` must be a vector, not `NULL`.
#> ℹ Read our FAQ about scalar types (`?vctrs::faq_error_scalar_type`) to learn more.
try(list_check_all_size(list(1, NULL, 2), size = 1))
#> Error in eval(expr, envir) : 
#>   `list(1, NULL, 2)[[2]]` must have size 1, not size 0.

# However, it is often useful to perform upfront vector/size checks on a
# list, excluding `NULL`s, and then filter them out later on
list_check_all_vectors(list(1, NULL, 2), allow_null = TRUE)
list_check_all_size(list(1, NULL, 2), size = 1, allow_null = TRUE)

# Performing the checks before removing `NULL`s from the list ensures that
# any errors report the correct index. Note how the index is incorrect from a
# user's point of view if we filter out `NULL` too soon.
xs <- list(1, NULL, 2:3)
try(list_check_all_size(xs, size = 1, allow_null = TRUE))
#> Error in eval(expr, envir) : `xs[[3]]` must have size 1, not size 2.
xs <- vec_slice(xs, !vec_detect_missing(xs))
try(list_check_all_size(xs, size = 1))
#> Error in eval(expr, envir) : `xs[[2]]` must have size 1, not size 2.
```
