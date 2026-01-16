# Compute ranks

`vec_rank()` computes the sample ranks of a vector. For data frames,
ranks are computed along the rows, using all columns after the first to
break ties.

## Usage

``` r
vec_rank(
  x,
  ...,
  ties = c("min", "max", "sequential", "dense"),
  incomplete = c("rank", "na"),
  direction = "asc",
  na_value = "largest",
  nan_distinct = FALSE,
  chr_proxy_collate = NULL
)
```

## Arguments

- x:

  A vector

- ...:

  These dots are for future extensions and must be empty.

- ties:

  Ranking of duplicate values.

  - `"min"`: Use the current rank for all duplicates. The next
    non-duplicate value will have a rank incremented by the number of
    duplicates present.

  - `"max"`: Use the current rank `+ n_duplicates - 1` for all
    duplicates. The next non-duplicate value will have a rank
    incremented by the number of duplicates present.

  - `"sequential"`: Use an increasing sequence of ranks starting at the
    current rank, applied to duplicates in order of appearance.

  - `"dense"`: Use the current rank for all duplicates. The next
    non-duplicate value will have a rank incremented by `1`, effectively
    removing any gaps in the ranking.

- incomplete:

  Ranking of missing and
  [incomplete](https://vctrs.r-lib.org/reference/vec_detect_complete.md)
  observations.

  - `"rank"`: Rank incomplete observations normally. Missing values
    within incomplete observations will be affected by `na_value` and
    `nan_distinct`.

  - `"na"`: Don't rank incomplete observations at all. Instead, they are
    given a rank of `NA`. In this case, `na_value` and `nan_distinct`
    have no effect.

- direction:

  Direction to sort in.

  - A single `"asc"` or `"desc"` for ascending or descending order
    respectively.

  - For data frames, a length `1` or `ncol(x)` character vector
    containing only `"asc"` or `"desc"`, specifying the direction for
    each column.

- na_value:

  Ordering of missing values.

  - A single `"largest"` or `"smallest"` for ordering missing values as
    the largest or smallest values respectively.

  - For data frames, a length `1` or `ncol(x)` character vector
    containing only `"largest"` or `"smallest"`, specifying how missing
    values should be ordered within each column.

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

## Details

Unlike [`base::rank()`](https://rdrr.io/r/base/rank.html), when
`incomplete = "rank"` all missing values are given the same rank, rather
than an increasing sequence of ranks. When `nan_distinct = FALSE`, `NaN`
values are given the same rank as `NA`, otherwise they are given a rank
that differentiates them from `NA`.

Like
[`vec_order_radix()`](https://vctrs.r-lib.org/reference/order-radix.md),
ordering is done in the C-locale. This can affect the ranks of character
vectors, especially regarding how uppercase and lowercase letters are
ranked. See the documentation of
[`vec_order_radix()`](https://vctrs.r-lib.org/reference/order-radix.md)
for more information.

## Dependencies

- [`vec_order_radix()`](https://vctrs.r-lib.org/reference/order-radix.md)

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

## Examples

``` r
x <- c(5L, 6L, 3L, 3L, 5L, 3L)

vec_rank(x, ties = "min")
#> [1] 4 6 1 1 4 1
vec_rank(x, ties = "max")
#> [1] 5 6 3 3 5 3

# Sequential ranks use an increasing sequence for duplicates
vec_rank(x, ties = "sequential")
#> [1] 4 6 1 2 5 3

# Dense ranks remove gaps between distinct values,
# even if there are duplicates
vec_rank(x, ties = "dense")
#> [1] 2 3 1 1 2 1

y <- c(NA, x, NA, NaN)

# Incomplete values match other incomplete values by default, and their
# overall position can be adjusted with `na_value`
vec_rank(y, na_value = "largest")
#> [1] 7 4 6 1 1 4 1 7 7
vec_rank(y, na_value = "smallest")
#> [1] 1 7 9 4 4 7 4 1 1

# NaN can be ranked separately from NA if required
vec_rank(y, nan_distinct = TRUE)
#> [1] 8 4 6 1 1 4 1 8 7

# Rank in descending order. Since missing values are the largest value,
# they are given a rank of `1` when ranking in descending order.
vec_rank(y, direction = "desc", na_value = "largest")
#> [1] 1 5 4 7 7 5 7 1 1

# Give incomplete values a rank of `NA` by setting `incomplete = "na"`
vec_rank(y, incomplete = "na")
#> [1] NA  4  6  1  1  4  1 NA NA

# Can also rank data frames, using columns after the first to break ties
z <- c(2L, 3L, 4L, 4L, 5L, 2L)
df <- data_frame(x = x, z = z)
df
#> # A tibble: 6 × 2
#>       x     z
#>   <int> <int>
#> 1     5     2
#> 2     6     3
#> 3     3     4
#> 4     3     4
#> 5     5     5
#> 6     3     2

vec_rank(df)
#> [1] 4 6 2 2 5 1
```
