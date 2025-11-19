# Parallel `any()` and `all()`

These functions are variants of
[`any()`](https://rdrr.io/r/base/any.html) and
[`all()`](https://rdrr.io/r/base/all.html) that work in parallel on
multiple inputs at once. They work similarly to how
[`pmin()`](https://rdrr.io/r/base/Extremes.html) and
[`pmax()`](https://rdrr.io/r/base/Extremes.html) are parallel variants
of [`min()`](https://rdrr.io/r/base/Extremes.html) and
[`max()`](https://rdrr.io/r/base/Extremes.html).

## Usage

``` r
vec_pany(
  ...,
  .missing = NA,
  .size = NULL,
  .arg = "",
  .error_call = current_env()
)

vec_pall(
  ...,
  .missing = NA,
  .size = NULL,
  .arg = "",
  .error_call = current_env()
)
```

## Arguments

- ...:

  Logical vectors of equal size.

- .missing:

  Value to use when a missing value is encountered. One of:

  - `NA` to propagate missing values. With this, missings are treated
    the same way as `|` or `&`.

  - `FALSE` to treat missing values as `FALSE`.

  - `TRUE` to treat missing values as `TRUE`.

- .size:

  An optional output size. Only useful to specify if it is possible for
  no inputs to be provided.

- .arg:

  Argument name used in error messages.

- .error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A logical vector the same size as the vectors in `...`.

## Details

`vec_pany()` and `vec_pall()` are consistent with
[`any()`](https://rdrr.io/r/base/any.html) and
[`all()`](https://rdrr.io/r/base/all.html) when there are no inputs to
process in parallel:

- [`any()`](https://rdrr.io/r/base/any.html) returns `FALSE` with no
  inputs. Similarly, `vec_pany(.size = 1)` returns `FALSE`.

- [`all()`](https://rdrr.io/r/base/all.html) returns `TRUE` with no
  inputs. Similarly, `vec_pall(.size = 1)` returns `TRUE`.

## Examples

``` r
a <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, NA, NA, NA)
b <- c(TRUE, FALSE, NA, TRUE, FALSE, NA, TRUE, FALSE, NA)

# Default behavior treats missings like `|` does
vec_pany(a, b)
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE    NA  TRUE    NA    NA
a | b
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE    NA  TRUE    NA    NA

# Default behavior treats missings like `&` does
vec_pall(a, b)
#> [1]  TRUE FALSE    NA FALSE FALSE FALSE    NA FALSE    NA
a & b
#> [1]  TRUE FALSE    NA FALSE FALSE FALSE    NA FALSE    NA

# Remove missings from the computation, like `na_rm = TRUE`
vec_pany(a, b, .missing = FALSE)
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE
(a & !is.na(a)) | (b & !is.na(b))
#> [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE

vec_pall(a, b, .missing = TRUE)
#> [1]  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE
(a | is.na(a)) & (b | is.na(b))
#> [1]  TRUE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE  TRUE

# `vec_pall()` can be used to implement a `dplyr::filter()` style API
df <- data_frame(id = seq_along(a), a = a, b = b)

keep_rows <- function(x, ...) {
  vec_slice(x, vec_pall(..., .missing = FALSE))
}
drop_rows <- function(x, ...) {
  vec_slice(x, !vec_pall(..., .missing = FALSE))
}

# "Keep / Drop the rows when both a and b are TRUE"
# These form complements of one another, even with `NA`s.
keep_rows(df, a, b)
#>   id    a    b
#> 1  1 TRUE TRUE
drop_rows(df, a, b)
#>   id     a     b
#> 1  2  TRUE FALSE
#> 2  3  TRUE    NA
#> 3  4 FALSE  TRUE
#> 4  5 FALSE FALSE
#> 5  6 FALSE    NA
#> 6  7    NA  TRUE
#> 7  8    NA FALSE
#> 8  9    NA    NA

# Same empty behavior as `any()` and `all()`
vec_pany(.size = 1)
#> [1] FALSE
any()
#> [1] FALSE

vec_pall(.size = 1)
#> [1] TRUE
all()
#> [1] TRUE
```
