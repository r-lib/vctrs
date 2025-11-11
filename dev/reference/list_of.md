# `list_of` S3 class for homogenous lists

A `list_of` object is a list where each element has the same type.
Modifying the list with `$`, `[`, and `[[` preserves the constraint by
coercing all input items.

## Usage

``` r
list_of(..., .ptype = NULL)

as_list_of(x, ...)

is_list_of(x)

# S3 method for class 'vctrs_list_of'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'vctrs_list_of'
vec_cast(x, to, ...)
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

- x:

  For `as_list_of()`, a vector to be coerced to list_of.

- y, to:

  Arguments to
  [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
  and [`vec_cast()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md).

- x_arg, y_arg:

  Argument names for `x` and `y`. These are used in error messages to
  inform the user about the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)).

## Details

Unlike regular lists, setting a list element to `NULL` using `[[` does
not remove it.

## Examples

``` r
x <- list_of(1:3, 5:6, 10:15)
if (requireNamespace("tibble", quietly = TRUE)) {
  tibble::tibble(x = x)
}
#> # A tibble: 3 × 1
#>             x
#>   <list<int>>
#> 1         [3]
#> 2         [2]
#> 3         [6]

vec_c(list_of(1, 2), list_of(FALSE, TRUE))
#> <list_of<double>[4]>
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 0
#> 
#> [[4]]
#> [1] 1
#> 
```
