# Recode and replace values

- `vec_recode_values()` constructs an entirely new vector by recoding
  the values from `x` specified in `from` to the corresponding values in
  `to`. If there are values in `x` not matched by `from`, then they are
  recoded to the `default` value.

- `vec_replace_values()` updates an existing vector by replacing the
  values from `x` specified in `from` with the corresponding values in
  `to`. In this case, `to` must have the same type as `x` and values in
  `x` not matched by `from` pass through untouched.

## Usage

``` r
vec_recode_values(
  x,
  ...,
  from,
  to,
  default = NULL,
  unmatched = "default",
  from_as_list_of_vectors = FALSE,
  to_as_list_of_vectors = FALSE,
  ptype = NULL,
  x_arg = "x",
  from_arg = "from",
  to_arg = "to",
  default_arg = "default",
  error_call = current_env()
)

vec_replace_values(
  x,
  ...,
  from,
  to,
  from_as_list_of_vectors = FALSE,
  to_as_list_of_vectors = FALSE,
  x_arg = "x",
  from_arg = "from",
  to_arg = "to",
  error_call = current_env()
)
```

## Arguments

- x:

  A vector.

- ...:

  These dots are for future extensions and must be empty.

- from:

  Values to locate in `x` and map to values in `to`.

  Extra values present in `from` but not in `x` are ignored.

  - If `from_as_list_of_vectors = FALSE`, `from` must be a single vector
    of any size, which will be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.md) to
    the type of `x`.

  - If `from_as_list_of_vectors = TRUE`, `from` must be a list of
    vectors of any size, which will individually be
    [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.md) to
    the type of `x`.

- to:

  Values to map `from` to.

  The common type of `to` and `default` will determine the type of the
  output, unless `ptype` is provided.

  - If `to_as_list_of_vectors = FALSE`, `to` must be a single vector of
    size 1 or the same size as `from`.

  - If `to_as_list_of_vectors = TRUE`, `to` must be a list of vectors.
    The list itself must be size 1 or the same size as `from`. Each
    individual vector in the list must be size 1 or the same size as
    `x`.

- default:

  Default value to use when there is a value present in `x` that is
  unmatched by a value in `from`.

  By default, a missing value is used as the default value.

  If supplied, `default` must be size 1 or the same size as `x`.

  Can only be set when `unmatched = "default"`.

- unmatched:

  Handling of unmatched locations.

  One of:

  - `"default"` to use `default` in unmatched locations.

  - `"error"` to error when there are unmatched locations.

- from_as_list_of_vectors, to_as_list_of_vectors:

  Boolean values determining whether to treat `from` and `to` as vectors
  or as lists of vectors. See their parameter descriptions for more
  details.

- ptype:

  An optional override for the output type, which is usually computed as
  the common type of `to` and `default`.

- x_arg, from_arg, to_arg, default_arg:

  Argument names used in error messages.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A vector the same size as `x`.

- For `vec_recode_values()`, the type of the output is computed as the
  common type of `to` and `default`, unless overridden by `ptype`. The
  names of the output come from the names of `to` and `default`.

- For `vec_replace_values()`, the type of the output will have the same
  type as `x`. The names of the output will be the same as the names of
  `x`.

## Examples

``` r
x <- c(1, 2, 3, 1, 2, 4, NA, 5)

# Imagine you have a pre-existing lookup table
likert <- data.frame(
  from = c(1, 2, 3, 4, 5),
  to = c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree"
  )
)
vec_recode_values(x, from = likert$from, to = likert$to)
#> [1] "Strongly disagree" "Disagree"          "Neutral"          
#> [4] "Strongly disagree" "Disagree"          "Agree"            
#> [7] NA                  "Strongly agree"   

# If you don't map all of the values, a `default` is used
x <- c(1, 2, 3, 1, 2, 4, NA, 5, 6, 7)
vec_recode_values(x, from = likert$from, to = likert$to)
#>  [1] "Strongly disagree" "Disagree"          "Neutral"          
#>  [4] "Strongly disagree" "Disagree"          "Agree"            
#>  [7] NA                  "Strongly agree"    NA                 
#> [10] NA                 
vec_recode_values(x, from = likert$from, to = likert$to, default = "Unknown")
#>  [1] "Strongly disagree" "Disagree"          "Neutral"          
#>  [4] "Strongly disagree" "Disagree"          "Agree"            
#>  [7] "Unknown"           "Strongly agree"    "Unknown"          
#> [10] "Unknown"          

# If you want existing `NA`s to pass through, include a mapping for `NA` in
# your lookup table
likert <- data.frame(
  from = c(1, 2, 3, 4, 5, NA),
  to = c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree",
    NA
  )
)
vec_recode_values(x, from = likert$from, to = likert$to, default = "Unknown")
#>  [1] "Strongly disagree" "Disagree"          "Neutral"          
#>  [4] "Strongly disagree" "Disagree"          "Agree"            
#>  [7] NA                  "Strongly agree"    "Unknown"          
#> [10] "Unknown"          

# If you believe you've captured all of the cases, you can assert this with
# `unmatched = "error"`, which will error if you've missed any cases
# (including `NA`, which must be explicitly handled)
try(vec_recode_values(
  x,
  from = likert$from,
  to = likert$to,
  unmatched = "error"
))
#> Error in vec_recode_values(x, from = likert$from, to = likert$to, unmatched = "error") : 
#>   Each location must be matched.
#> ✖ Locations 9 and 10 are unmatched.

if (require("tibble")) {
  # If you want to partially update `x`, retaining the type of `x` and
  # leaving values not covered by `from` alone, use `vec_replace_values()`
  universities <- c(
    "Duke",
    "Fake U",
    "Duke U",
    NA,
    "Chapel Hill",
    "UNC",
    NA,
    "Duke"
  )

  standardize <- tibble::tribble(
    ~from, ~to,
    "Duke", "Duke University",
    "Duke U", "Duke University",
    "UNC", "UNC Chapel Hill",
    "Chapel Hill", "UNC Chapel Hill",
  )
  vec_replace_values(
    universities,
    from = standardize$from,
    to = standardize$to
  )

  # In this case, you can use a more powerful feature of
  # `vec_replace_values()`, `from_as_list_of_vectors`, which allows you to
  # provide a list of `from` vectors that each match multiple `from` values
  # to a single `to` value. `tribble()` can help you create these!
  standardize <- tibble::tribble(
    ~from, ~to,
    c("Duke", "Duke U"), "Duke University",
    c("UNC", "Chapel Hill"), "UNC Chapel Hill",
  )

  # Note how `from` is a list column
  standardize

  vec_replace_values(
    universities,
    from = standardize$from,
    to = standardize$to,
    from_as_list_of_vectors = TRUE
  )

  # `vec_replace_values()` is also a useful way to map from or to `NA`
  vec_replace_values(universities, from = NA, to = "Unknown")
  vec_replace_values(universities, from = "Fake U", to = NA)
}
#> Loading required package: tibble
#> 
#> Attaching package: ‘tibble’
#> The following object is masked from ‘package:vctrs’:
#> 
#>     data_frame
#> [1] "Duke"        NA            "Duke U"      NA           
#> [5] "Chapel Hill" "UNC"         NA            "Duke"       
```
