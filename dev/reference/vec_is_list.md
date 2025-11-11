# List checks

**\[deprecated\]**

These functions have been deprecated as of vctrs 0.6.0.

- `vec_is_list()` has been renamed to
  [`obj_is_list()`](https://vctrs.r-lib.org/dev/reference/obj_is_list.md).

- `vec_check_list()` has been renamed to
  [`obj_check_list()`](https://vctrs.r-lib.org/dev/reference/obj_is_list.md).

## Usage

``` r
vec_is_list(x)

vec_check_list(x, ..., arg = caller_arg(x), call = caller_env())
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
