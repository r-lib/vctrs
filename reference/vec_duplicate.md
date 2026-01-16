# Find duplicated values

- `vec_duplicate_any()`: detects the presence of duplicated values,
  similar to
  [`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html).

- `vec_duplicate_detect()`: returns a logical vector describing if each
  element of the vector is duplicated elsewhere. Unlike
  [`duplicated()`](https://rdrr.io/r/base/duplicated.html), it reports
  all duplicated values, not just the second and subsequent repetitions.

- `vec_duplicate_id()`: returns an integer vector giving the location of
  the first occurrence of the value.

## Usage

``` r
vec_duplicate_any(x)

vec_duplicate_detect(x)

vec_duplicate_id(x)
```

## Arguments

- x:

  A vector (including a data frame).

## Value

- `vec_duplicate_any()`: a logical vector of length 1.

- `vec_duplicate_detect()`: a logical vector the same length as `x`.

- `vec_duplicate_id()`: an integer vector the same length as `x`.

## Missing values

In most cases, missing values are not considered to be equal, i.e.
`NA == NA` is not `TRUE`. This behaviour would be unappealing here, so
these functions consider all `NAs` to be equal. (Similarly, all `NaN`
are also considered to be equal.)

## Dependencies

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.md)

## See also

[`vec_unique()`](https://vctrs.r-lib.org/reference/vec_unique.md) for
functions that work with the dual of duplicated values: unique values.

## Examples

``` r
vec_duplicate_any(1:10)
#> [1] FALSE
vec_duplicate_any(c(1, 1:10))
#> [1] TRUE

x <- c(10, 10, 20, 30, 30, 40)
vec_duplicate_detect(x)
#> [1]  TRUE  TRUE FALSE  TRUE  TRUE FALSE
# Note that `duplicated()` doesn't consider the first instance to
# be a duplicate
duplicated(x)
#> [1] FALSE  TRUE FALSE FALSE  TRUE FALSE

# Identify elements of a vector by the location of the first element that
# they're equal to:
vec_duplicate_id(x)
#> [1] 1 1 3 4 4 6
# Location of the unique values:
vec_unique_loc(x)
#> [1] 1 3 4 6
# Equivalent to `duplicated()`:
vec_duplicate_id(x) == seq_along(x)
#> [1]  TRUE FALSE  TRUE  TRUE FALSE  TRUE
```
