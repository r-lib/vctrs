# Runs

- `vec_identify_runs()` returns a vector of identifiers for the elements
  of `x` that indicate which run of repeated values they fall in. The
  number of runs is also returned as an attribute, `n`.

- `vec_run_sizes()` returns an integer vector corresponding to the size
  of each run. This is identical to the `times` column from
  [`vec_unrep()`](https://vctrs.r-lib.org/dev/reference/vec-rep.md), but
  is faster if you don't need the run keys.

- [`vec_unrep()`](https://vctrs.r-lib.org/dev/reference/vec-rep.md) is a
  generalized [`base::rle()`](https://rdrr.io/r/base/rle.html). It is
  documented alongside the "repeat" functions of
  [`vec_rep()`](https://vctrs.r-lib.org/dev/reference/vec-rep.md) and
  [`vec_rep_each()`](https://vctrs.r-lib.org/dev/reference/vec-rep.md);
  look there for more information.

## Usage

``` r
vec_identify_runs(x)

vec_run_sizes(x)
```

## Arguments

- x:

  A vector.

## Value

- For `vec_identify_runs()`, an integer vector with the same size as
  `x`. A scalar integer attribute, `n`, is attached.

- For `vec_run_sizes()`, an integer vector with size equal to the number
  of runs in `x`.

## Details

Unlike [`base::rle()`](https://rdrr.io/r/base/rle.html), adjacent
missing values are considered identical when constructing runs. For
example, `vec_identify_runs(c(NA, NA))` will return `c(1, 1)`, not
`c(1, 2)`.

## See also

[`vec_unrep()`](https://vctrs.r-lib.org/dev/reference/vec-rep.md) for a
generalized [`base::rle()`](https://rdrr.io/r/base/rle.html).

## Examples

``` r
x <- c("a", "z", "z", "c", "a", "a")

vec_identify_runs(x)
#> [1] 1 2 2 3 4 4
#> attr(,"n")
#> [1] 4
vec_run_sizes(x)
#> [1] 1 2 1 2
vec_unrep(x)
#>   key times
#> 1   a     1
#> 2   z     2
#> 3   c     1
#> 4   a     2

y <- c(1, 1, 1, 2, 2, 3)

# With multiple columns, the runs are constructed rowwise
df <- data_frame(
  x = x,
  y = y
)

vec_identify_runs(df)
#> [1] 1 2 2 3 4 5
#> attr(,"n")
#> [1] 5
vec_run_sizes(df)
#> [1] 1 2 1 1 1
vec_unrep(df)
#>   key.x key.y times
#> 1     a     1     1
#> 2     z     1     2
#> 3     c     2     1
#> 4     a     2     1
#> 5     a     3     1
```
