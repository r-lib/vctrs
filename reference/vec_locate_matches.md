# Locate observations matching specified conditions

`vec_locate_matches()` is a more flexible version of
[`vec_match()`](https://vctrs.r-lib.org/reference/vec_match.md) used to
identify locations where each value of `needles` matches one or multiple
values in `haystack`. Unlike
[`vec_match()`](https://vctrs.r-lib.org/reference/vec_match.md),
`vec_locate_matches()` returns all matches by default, and can match on
binary conditions other than equality, such as `>`, `>=`, `<`, and `<=`.

## Usage

``` r
vec_locate_matches(
  needles,
  haystack,
  ...,
  condition = "==",
  filter = "none",
  incomplete = "compare",
  no_match = NA_integer_,
  remaining = "drop",
  multiple = "all",
  relationship = "none",
  nan_distinct = FALSE,
  chr_proxy_collate = NULL,
  needles_arg = "needles",
  haystack_arg = "haystack",
  error_call = current_env()
)
```

## Arguments

- needles, haystack:

  Vectors used for matching.

  - `needles` represents the vector to search for.

  - `haystack` represents the vector to search in.

  Prior to comparison, `needles` and `haystack` are coerced to the same
  type.

- ...:

  These dots are for future extensions and must be empty.

- condition:

  Condition controlling how `needles` should be compared against
  `haystack` to identify a successful match.

  - One of: `"=="`, `">"`, `">="`, `"<"`, or `"<="`.

  - For data frames, a length `1` or `ncol(needles)` character vector
    containing only the above options, specifying how matching is
    determined for each column.

- filter:

  Filter to be applied to the matched results.

  - `"none"` doesn't apply any filter.

  - `"min"` returns only the minimum haystack value matching the current
    needle.

  - `"max"` returns only the maximum haystack value matching the current
    needle.

  - For data frames, a length `1` or `ncol(needles)` character vector
    containing only the above options, specifying a filter to apply to
    each column.

  Filters don't have any effect on `"=="` conditions, but are useful for
  computing "rolling" matches with other conditions.

  A filter can return multiple haystack matches for a particular needle
  if the maximum or minimum haystack value is duplicated in `haystack`.
  These can be further controlled with `multiple`.

- incomplete:

  Handling of missing and
  [incomplete](https://vctrs.r-lib.org/reference/vec_detect_complete.md)
  values in `needles`.

  - `"compare"` uses `condition` to determine whether or not a missing
    value in `needles` matches a missing value in `haystack`. If
    `condition` is `==`, `>=`, or `<=`, then missing values will match.

  - `"match"` always allows missing values in `needles` to match missing
    values in `haystack`, regardless of the `condition`.

  - `"drop"` drops incomplete values in `needles` from the result.

  - `"error"` throws an error if any `needles` are incomplete.

  - If a single integer is provided, this represents the value returned
    in the `haystack` column for values of `needles` that are
    incomplete. If `no_match = NA`, setting `incomplete = NA` forces
    incomplete values in `needles` to be treated like unmatched values.

  `nan_distinct` determines whether a `NA` is allowed to match a `NaN`.

- no_match:

  Handling of `needles` without a match.

  - `"drop"` drops `needles` with zero matches from the result.

  - `"error"` throws an error if any `needles` have zero matches.

  - If a single integer is provided, this represents the value returned
    in the `haystack` column for values of `needles` that have zero
    matches. The default represents an unmatched needle with `NA`.

- remaining:

  Handling of `haystack` values that `needles` never matched.

  - `"drop"` drops remaining `haystack` values from the result.
    Typically, this is the desired behavior if you only care when
    `needles` has a match.

  - `"error"` throws an error if there are any remaining `haystack`
    values.

  - If a single integer is provided (often `NA`), this represents the
    value returned in the `needles` column for the remaining `haystack`
    values that `needles` never matched. Remaining `haystack` values are
    always returned at the end of the result.

- multiple:

  Handling of `needles` with multiple matches. For each needle:

  - `"all"` returns all matches detected in `haystack`.

  - `"any"` returns any match detected in `haystack` with no guarantees
    on which match will be returned. It is often faster than `"first"`
    and `"last"` if you just need to detect if there is at least one
    match.

  - `"first"` returns the first match detected in `haystack`.

  - `"last"` returns the last match detected in `haystack`.

- relationship:

  Handling of the expected relationship between `needles` and
  `haystack`. If the expectations chosen from the list below are
  invalidated, an error is thrown.

  - `"none"` doesn't perform any relationship checks.

  - `"one-to-one"` expects:

    - Each value in `needles` matches at most 1 value in `haystack`.

    - Each value in `haystack` matches at most 1 value in `needles`.

  - `"one-to-many"` expects:

    - Each value in `needles` matches any number of values in
      `haystack`.

    - Each value in `haystack` matches at most 1 value in `needles`.

  - `"many-to-one"` expects:

    - Each value in `needles` matches at most 1 value in `haystack`.

    - Each value in `haystack` matches any number of values in
      `needles`.

  - `"many-to-many"` expects:

    - Each value in `needles` matches any number of values in
      `haystack`.

    - Each value in `haystack` matches any number of values in
      `needles`.

    This performs no checks, and is identical to `"none"`, but is
    provided to allow you to be explicit about this relationship if you
    know it exists.

  - `"warn-many-to-many"` doesn't assume there is any known
    relationship, but will warn if `needles` and `haystack` have a
    many-to-many relationship (which is typically unexpected),
    encouraging you to either take a closer look at your inputs or make
    this relationship explicit by specifying `"many-to-many"`.

  `relationship` is applied after `filter` and `multiple` to allow
  potential multiple matches to be filtered out first.

  `relationship` doesn't handle cases where there are zero matches. For
  that, see `no_match` and `remaining`.

- nan_distinct:

  A single logical specifying whether or not `NaN` should be considered
  distinct from `NA` for double and complex vectors. If `TRUE`, `NaN`
  will always be ordered between `NA` and non-missing numbers.

- chr_proxy_collate:

  A function generating an alternate representation of character vectors
  to use for collation, often used for locale-aware ordering.

  - If `NULL`, no transformation is done.

  - Otherwise, this must be a function of one argument. If the input
    contains a character vector, it will be passed to this function
    after it has been translated to UTF-8. This function should return a
    character vector with the same length as the input. The result
    should sort as expected in the C-locale, regardless of encoding.

  For data frames, `chr_proxy_collate` will be applied to all character
  columns.

  Common transformation functions include:
  [`tolower()`](https://rdrr.io/r/base/chartr.html) for case-insensitive
  ordering and
  [`stringi::stri_sort_key()`](https://rdrr.io/pkg/stringi/man/stri_sort_key.html)
  for locale-aware ordering.

- needles_arg, haystack_arg:

  Argument tags for `needles` and `haystack` used in error messages.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A two column data frame containing the locations of the matches.

- `needles` is an integer vector containing the location of the needle
  currently being matched.

- `haystack` is an integer vector containing the location of the
  corresponding match in the haystack for the current needle.

## Details

[`vec_match()`](https://vctrs.r-lib.org/reference/vec_match.md) is
identical to (but often slightly faster than):

    vec_locate_matches(
      needles,
      haystack,
      condition = "==",
      multiple = "first",
      nan_distinct = TRUE
    )

`vec_locate_matches()` is extremely similar to a SQL join between
`needles` and `haystack`, with the default being most similar to a left
join.

Be very careful when specifying match `condition`s. If a condition is
misspecified, it is very easy to accidentally generate an exponentially
large number of matches.

## Dependencies of `vec_locate_matches()`

- [`vec_order_radix()`](https://vctrs.r-lib.org/reference/order-radix.md)

- [`vec_detect_complete()`](https://vctrs.r-lib.org/reference/vec_detect_complete.md)

## Examples

``` r
x <- c(1, 2, NA, 3, NaN)
y <- c(2, 1, 4, NA, 1, 2, NaN)

# By default, for each value of `x`, all matching locations in `y` are
# returned
matches <- vec_locate_matches(x, y)
matches
#>   needles haystack
#> 1       1        2
#> 2       1        5
#> 3       2        1
#> 4       2        6
#> 5       3        4
#> 6       3        7
#> 7       4       NA
#> 8       5        4
#> 9       5        7

# The result can be used to slice the inputs to align them
data_frame(
  x = vec_slice(x, matches$needles),
  y = vec_slice(y, matches$haystack)
)
#> # A tibble: 9 × 2
#>       x     y
#>   <dbl> <dbl>
#> 1     1     1
#> 2     1     1
#> 3     2     2
#> 4     2     2
#> 5    NA    NA
#> 6    NA   NaN
#> 7     3    NA
#> 8   NaN    NA
#> 9   NaN   NaN

# If multiple matches are present, control which is returned with `multiple`
vec_locate_matches(x, y, multiple = "first")
#>   needles haystack
#> 1       1        2
#> 2       2        1
#> 3       3        4
#> 4       4       NA
#> 5       5        4
vec_locate_matches(x, y, multiple = "last")
#>   needles haystack
#> 1       1        5
#> 2       2        6
#> 3       3        7
#> 4       4       NA
#> 5       5        7
vec_locate_matches(x, y, multiple = "any")
#>   needles haystack
#> 1       1        2
#> 2       2        1
#> 3       3        4
#> 4       4       NA
#> 5       5        4

# Use `relationship` to add constraints and error on multiple matches if
# they aren't expected
try(vec_locate_matches(x, y, relationship = "one-to-one"))
#> Error in vec_locate_matches(x, y, relationship = "one-to-one") : 
#>   Each value of `needles` can match at most 1 value from `haystack`.
#> ✖ Location 1 of `needles` matches multiple values.

# In this case, the `NA` in `y` matches two rows in `x`
try(vec_locate_matches(x, y, relationship = "one-to-many"))
#> Error in vec_locate_matches(x, y, relationship = "one-to-many") : 
#>   Each value of `haystack` can match at most 1 value from `needles`.
#> ✖ Location 4 of `haystack` matches multiple values.

# By default, `NA` is treated as being identical to `NaN`.
# Using `nan_distinct = TRUE` treats `NA` and `NaN` as different values, so
# `NA` can only match `NA`, and `NaN` can only match `NaN`.
vec_locate_matches(x, y, nan_distinct = TRUE)
#>   needles haystack
#> 1       1        2
#> 2       1        5
#> 3       2        1
#> 4       2        6
#> 5       3        4
#> 6       4       NA
#> 7       5        7

# If you never want missing values to match, set `incomplete = NA` to return
# `NA` in the `haystack` column anytime there was an incomplete value
# in `needles`.
vec_locate_matches(x, y, incomplete = NA)
#>   needles haystack
#> 1       1        2
#> 2       1        5
#> 3       2        1
#> 4       2        6
#> 5       3       NA
#> 6       4       NA
#> 7       5       NA

# Using `incomplete = NA` allows us to enforce the one-to-many relationship
# that we couldn't before
vec_locate_matches(x, y, relationship = "one-to-many", incomplete = NA)
#>   needles haystack
#> 1       1        2
#> 2       1        5
#> 3       2        1
#> 4       2        6
#> 5       3       NA
#> 6       4       NA
#> 7       5       NA

# `no_match` allows you to specify the returned value for a needle with
# zero matches. Note that this is different from an incomplete value,
# so specifying `no_match` allows you to differentiate between incomplete
# values and unmatched values.
vec_locate_matches(x, y, incomplete = NA, no_match = 0L)
#>   needles haystack
#> 1       1        2
#> 2       1        5
#> 3       2        1
#> 4       2        6
#> 5       3       NA
#> 6       4        0
#> 7       5       NA

# If you want to require that every `needle` has at least 1 match, set
# `no_match` to `"error"`:
try(vec_locate_matches(x, y, incomplete = NA, no_match = "error"))
#> Error in vec_locate_matches(x, y, incomplete = NA, no_match = "error") : 
#>   Each value of `needles` must have a match in `haystack`.
#> ✖ Location 4 of `needles` does not have a match.

# By default, `vec_locate_matches()` detects equality between `needles` and
# `haystack`. Using `condition`, you can detect where an inequality holds
# true instead. For example, to find every location where `x[[i]] >= y`:
matches <- vec_locate_matches(x, y, condition = ">=")

data_frame(
  x = vec_slice(x, matches$needles),
  y = vec_slice(y, matches$haystack)
)
#> # A tibble: 14 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1     1     1
#>  2     1     1
#>  3     2     2
#>  4     2     1
#>  5     2     1
#>  6     2     2
#>  7    NA    NA
#>  8    NA   NaN
#>  9     3     2
#> 10     3     1
#> 11     3     1
#> 12     3     2
#> 13   NaN    NA
#> 14   NaN   NaN

# You can limit which matches are returned with a `filter`. For example,
# with the above example you can filter the matches returned by `x[[i]] >= y`
# down to only the ones containing the maximum `y` value of those matches.
matches <- vec_locate_matches(x, y, condition = ">=", filter = "max")

# Here, the matches for the `3` needle value have been filtered down to
# only include the maximum haystack value of those matches, `2`. This is
# often referred to as a rolling join.
data_frame(
  x = vec_slice(x, matches$needles),
  y = vec_slice(y, matches$haystack)
)
#> # A tibble: 10 × 2
#>        x     y
#>    <dbl> <dbl>
#>  1     1     1
#>  2     1     1
#>  3     2     2
#>  4     2     2
#>  5    NA    NA
#>  6    NA   NaN
#>  7     3     2
#>  8     3     2
#>  9   NaN    NA
#> 10   NaN   NaN

# In the very rare case that you need to generate locations for a
# cross match, where every value of `x` is forced to match every
# value of `y` regardless of what the actual values are, you can
# replace `x` and `y` with integer vectors of the same size that contain
# a single value and match on those instead.
x_proxy <- vec_rep(1L, vec_size(x))
y_proxy <- vec_rep(1L, vec_size(y))
nrow(vec_locate_matches(x_proxy, y_proxy))
#> [1] 35
vec_size(x) * vec_size(y)
#> [1] 35

# By default, missing values will match other missing values when using
# `==`, `>=`, or `<=` conditions, but not when using `>` or `<` conditions.
# This is similar to how `vec_compare(x, y, na_equal = TRUE)` works.
x <- c(1, NA)
y <- c(NA, 2)

vec_locate_matches(x, y, condition = "<=")
#>   needles haystack
#> 1       1        2
#> 2       2        1
vec_locate_matches(x, y, condition = "<")
#>   needles haystack
#> 1       1        2
#> 2       2       NA

# You can force missing values to match regardless of the `condition`
# by using `incomplete = "match"`
vec_locate_matches(x, y, condition = "<", incomplete = "match")
#>   needles haystack
#> 1       1        2
#> 2       2        1

# You can also use data frames for `needles` and `haystack`. The
# `condition` will be recycled to the number of columns in `needles`, or
# you can specify varying conditions per column. In this example, we take
# a vector of date `values` and find all locations where each value is
# between lower and upper bounds specified by the `haystack`.
values <- as.Date("2019-01-01") + 0:9
needles <- data_frame(lower = values, upper = values)

set.seed(123)
lower <- as.Date("2019-01-01") + sample(10, 10, replace = TRUE)
upper <- lower + sample(3, 10, replace = TRUE)
haystack <- data_frame(lower = lower, upper = upper)

# (values >= lower) & (values <= upper)
matches <- vec_locate_matches(needles, haystack, condition = c(">=", "<="))

data_frame(
  lower = vec_slice(lower, matches$haystack),
  value = vec_slice(values, matches$needle),
  upper = vec_slice(upper, matches$haystack)
)
#> # A tibble: 22 × 3
#>    lower      value      upper     
#>    <date>     <date>     <date>    
#>  1 NA         2019-01-01 NA        
#>  2 NA         2019-01-02 NA        
#>  3 2019-01-03 2019-01-03 2019-01-06
#>  4 2019-01-04 2019-01-04 2019-01-07
#>  5 2019-01-04 2019-01-04 2019-01-05
#>  6 2019-01-03 2019-01-04 2019-01-06
#>  7 2019-01-04 2019-01-05 2019-01-07
#>  8 2019-01-04 2019-01-05 2019-01-05
#>  9 2019-01-03 2019-01-05 2019-01-06
#> 10 2019-01-05 2019-01-05 2019-01-06
#> # ℹ 12 more rows
```
