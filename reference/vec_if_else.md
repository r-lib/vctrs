# Vectorized if-else

`vec_if_else()` is a vectorized
[if-else](https://rdrr.io/r/base/Control.html). Compared to the base R
equivalent, [`ifelse()`](https://rdrr.io/r/base/ifelse.html), this
function allows you to handle missing values in the `condition` with
`missing` and always takes `true`, `false`, and `missing` into account
when determining what the output type should be.

## Usage

``` r
vec_if_else(
  condition,
  true,
  false,
  ...,
  missing = NULL,
  ptype = NULL,
  condition_arg = "condition",
  true_arg = "true",
  false_arg = "false",
  missing_arg = "missing",
  error_call = current_env()
)
```

## Arguments

- condition:

  A logical vector.

- true, false:

  Vectors to use for `TRUE` and `FALSE` values of `condition`.

  Both `true` and `false` will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.md)
  to the size of `condition`.

  `true`, `false`, and `missing` (if used) will be cast to their common
  type.

- ...:

  These dots are for future extensions and must be empty.

- missing:

  If not `NULL`, will be used as the value for `NA` values of
  `condition`. Follows the same size and type rules as `true` and
  `false`.

- ptype:

  An optional prototype declaring the desired output type. If supplied,
  this overrides the common type of `true`, `false`, and `missing`.

- condition_arg, true_arg, false_arg, missing_arg:

  Argument names used in error messages.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A vector with the same size as `condition` and the same type as the
common type of `true`, `false`, and `missing`.

Where `condition` is `TRUE`, the matching values from `true`, where it
is `FALSE`, the matching values from `false`, and where it is `NA`, the
matching values from `missing`, if provided, otherwise a missing value
will be used.

## Examples

``` r
x <- c(-5:5, NA)
vec_if_else(x < 0, NA, x)
#>  [1] NA NA NA NA NA  0  1  2  3  4  5 NA

# Explicitly handle `NA` values in the `condition` with `missing`
vec_if_else(x < 0, "negative", "positive", missing = "missing")
#>  [1] "negative" "negative" "negative" "negative" "negative" "positive"
#>  [7] "positive" "positive" "positive" "positive" "positive" "missing" 

# Unlike `ifelse()`, `vec_if_else()` preserves types
x <- factor(sample(letters[1:5], 10, replace = TRUE))
ifelse(x %in% c("a", "b", "c"), x, NA)
#>  [1]  2 NA  2 NA NA  3 NA  1  1 NA
vec_if_else(x %in% c("a", "b", "c"), x, NA)
#>  [1] b    <NA> b    <NA> <NA> c    <NA> a    a    <NA>
#> Levels: a b c d e

# `vec_if_else()` also works with data frames
condition <- c(TRUE, FALSE, NA, TRUE)
true <- data_frame(x = 1:4, y = 5:8)
false <- data_frame(x = 9:12, y = 13:16)
vec_if_else(condition, true, false)
#> # A tibble: 4 × 2
#>       x     y
#>   <int> <int>
#> 1     1     5
#> 2    10    14
#> 3    NA    NA
#> 4     4     8
```
