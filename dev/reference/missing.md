# Missing values

- `vec_detect_missing()` returns a logical vector the same size as `x`.
  For each element of `x`, it returns `TRUE` if the element is missing,
  and `FALSE` otherwise.

- `vec_any_missing()` returns a single `TRUE` or `FALSE` depending on
  whether or not `x` has *any* missing values.

### Differences with [`is.na()`](https://rdrr.io/r/base/NA.html)

Data frame rows are only considered missing if every element in the row
is missing. Similarly, [record
vector](https://vctrs.r-lib.org/dev/reference/new_rcrd.md) elements are
only considered missing if every field in the record is missing. Put
another way, rows with *any* missing values are considered
[incomplete](https://vctrs.r-lib.org/dev/reference/vec_detect_complete.md),
but only rows with *all* missing values are considered missing.

List elements are only considered missing if they are `NULL`.

## Usage

``` r
vec_detect_missing(x)

vec_any_missing(x)
```

## Arguments

- x:

  A vector

## Value

- `vec_detect_missing()` returns a logical vector the same size as `x`.

- `vec_any_missing()` returns a single `TRUE` or `FALSE`.

## Dependencies

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md)

## See also

[`vec_detect_complete()`](https://vctrs.r-lib.org/dev/reference/vec_detect_complete.md)

## Examples

``` r
x <- c(1, 2, NA, 4, NA)

vec_detect_missing(x)
#> [1] FALSE FALSE  TRUE FALSE  TRUE
vec_any_missing(x)
#> [1] TRUE

# Data frames are iterated over rowwise, and only report a row as missing
# if every element of that row is missing. If a row is only partially
# missing, it is said to be incomplete, but not missing.
y <- c("a", "b", NA, "d", "e")
df <- data_frame(x = x, y = y)

df$missing <- vec_detect_missing(df)
df$incomplete <- !vec_detect_complete(df)
df
#>    x    y missing incomplete
#> 1  1    a   FALSE      FALSE
#> 2  2    b   FALSE      FALSE
#> 3 NA <NA>    TRUE       TRUE
#> 4  4    d   FALSE      FALSE
#> 5 NA    e   FALSE       TRUE
```
