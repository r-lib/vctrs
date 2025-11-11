# Recode and replace using logical conditions

- `vec_case_when()` constructs an entirely new vector by recoding the
  `TRUE` `conditions` to their corresponding `values`. If there are
  locations not matched by `conditions`, then they are recoded to the
  `default` value.

- `vec_replace_when()` updates an existing vector by replacing the
  values from `x` matched by the `TRUE` `conditions` with their
  corresponding `values`. In this case, each element of `values` must
  have the same type as `x` and locations not matched by `conditions`
  retain their original `x` value.

`vec_case_when()` is often thought of as a way to vectorize multiple
if-else statements, and is an R equivalent of the SQL "searched"
`CASE WHEN` statement.

## Usage

``` r
vec_case_when(
  conditions,
  values,
  ...,
  default = NULL,
  unmatched = "default",
  ptype = NULL,
  size = NULL,
  conditions_arg = "conditions",
  values_arg = "values",
  default_arg = "default",
  error_call = current_env()
)

vec_replace_when(
  x,
  conditions,
  values,
  ...,
  x_arg = "x",
  conditions_arg = "conditions",
  values_arg = "values",
  error_call = current_env()
)
```

## Arguments

- conditions:

  A list of logical condition vectors.

  For `vec_case_when()`, each vector should be the same size.

  For `vec_replace_when()`, each vector should be the same size as `x`.

  Where a value in `conditions` is `TRUE`, the corresponding value in
  `values` will be assigned to the result.

- values:

  A list of vectors.

  For `vec_case_when()`, each vector should be size 1 or the size
  implied by `conditions`. The common type of `values` and `default`
  determine the output type, unless overridden by `ptype`.

  For `vec_replace_when()`, each vector should be size 1 or the same
  size as `x`. Each vector will be cast to the type of `x`.

- ...:

  These dots are for future extensions and must be empty.

- default:

  Default value to use when `conditions` does not match every location
  in the output.

  By default, a missing value is used as the default value.

  If supplied, `default` must be size 1 or the size implied by
  `conditions`.

  Can only be set when `unmatched = "default"`.

- unmatched:

  Handling of unmatched locations.

  One of:

  - `"default"` to use `default` in unmatched locations.

  - `"error"` to error when there are unmatched locations.

- ptype:

  An optional override for the output type, which is usually computed as
  the common type of `values` and `default`.

- size:

  An optional override for the output size, which is usually computed as
  the size of the first element of `conditions`.

  Only useful for requiring a fixed size when `conditions` is an empty
  list.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- x:

  A vector.

- x_arg, conditions_arg, values_arg, default_arg:

  Argument names used in error messages.

## Value

A vector.

- For `vec_case_when()`, the type of the output is computed as the
  common type of `values` and `default`, unless overridden by `ptype`.
  The names of the output come from the names of `values` and `default`.
  The size of the output comes from the implied size from `conditions`,
  unless overridden by `size`.

- For `vec_replace_when()`, the type of the output will have the same
  type as `x`. The names of the output will be the same as the names of
  `x`. The size of the output will be the same size as `x`.

## Examples

``` r
# Note how the first `TRUE` is used in the output.
# Also note how the `NA` falls through to `default`.
x <- seq(-2L, 2L, by = 1L)
x <- c(x, NA)
conditions <- list(
  x < 0,
  x < 1
)
values <- list(
  "<0",
  "<1"
)
vec_case_when(
  conditions,
  values,
  default = "other"
)
#> [1] "<0"    "<0"    "<1"    "other" "other" "other"

# Missing values need to be handled with their own case
# if you want them to have a special value
conditions <- list(
  x < 0,
  x < 1,
  is.na(x)
)
values <- list(
  "<0",
  "<1",
  NA
)
vec_case_when(
  conditions,
  values,
  default = "other"
)
#> [1] "<0"    "<0"    "<1"    "other" "other" NA     

# Both `values` and `default` are vectorized
values <- list(
  x * 5,
  x * 10,
  NA
)
vec_case_when(
  conditions,
  values,
  default = x * 100
)
#> [1] -10  -5   0 100 200  NA

# Use `vec_replace_when()` if you need to update `x`, retaining
# all previous values in locations that you don't match
conditions <- list(
  x < 0,
  x < 1
)
values <- list(
  0,
  1
)
out <- vec_replace_when(
  x,
  conditions,
  values
)
out
#> [1]  0  0  1  1  2 NA

# Note how `vec_replace_when()` is type stable on `x`, we retain the
# integer type here even though `values` contained doubles
typeof(out)
#> [1] "integer"

# `vec_case_when()` creates a new vector, so names come from `values`
# and `default`. `vec_replace_when()` modifies an existing vector, so
# names come from `x` no matter what, just like `[<-` and `base::replace()`
x <- c(a = 1, b = 2, c = 3)
conditions <- list(x == 1, x == 2)
values <- list(c(x = 0), c(y = -1))
vec_case_when(conditions, values)
#>  x  y    
#>  0 -1 NA 
vec_replace_when(x, conditions, values)
#>  a  b  c 
#>  0 -1  3 

# If you want to enforce that you've covered all of the locations in your
# `conditions`, use `unmatched = "error"` rather than providing a `default`
x <- c(0, 1, 2)
conditions <- list(x == 1, x == 2)
values <- list("a", "b")
try(vec_case_when(conditions, values, unmatched = "error"))
#> Error in vec_case_when(conditions, values, unmatched = "error") : 
#>   Each location must be matched.
#> ✖ Location 1 is unmatched.
```
