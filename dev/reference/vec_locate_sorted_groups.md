# Locate sorted groups

**\[experimental\]**

`vec_locate_sorted_groups()` returns a data frame containing a `key`
column with sorted unique groups, and a `loc` column with the locations
of each group in `x`. It is similar to
[`vec_group_loc()`](https://vctrs.r-lib.org/dev/reference/vec_group.md),
except the groups are returned sorted rather than by first appearance.

## Usage

``` r
vec_locate_sorted_groups(
  x,
  ...,
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

## Value

A two column data frame with size equal to `vec_size(vec_unique(x))`.

- A `key` column of type `vec_ptype(x)`.

- A `loc` column of type list, with elements of type integer.

## Details

`vec_locate_sorted_groups(x)` is equivalent to, but faster than:

    info <- vec_group_loc(x)
    vec_slice(info, vec_order(info$key))

## Dependencies of `vec_locate_sorted_groups()`

- [`vec_proxy_order()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_compare.md)

## Examples

``` r
df <- data.frame(
  g = sample(2, 10, replace = TRUE),
  x = c(NA, sample(5, 9, replace = TRUE))
)

# `vec_locate_sorted_groups()` is similar to `vec_group_loc()`, except keys
# are returned ordered rather than by first appearance.
vec_locate_sorted_groups(df)
#>   key.g key.x  loc
#> 1     1     2    6
#> 2     1     3    7
#> 3     1     4    8
#> 4     1     5 3, 9
#> 5     1    NA    1
#> 6     2     1 4, 5
#> 7     2     2    2
#> 8     2     5   10

vec_group_loc(df)
#>   key.g key.x  loc
#> 1     1    NA    1
#> 2     2     2    2
#> 3     1     5 3, 9
#> 4     2     1 4, 5
#> 5     1     2    6
#> 6     1     3    7
#> 7     1     4    8
#> 8     2     5   10
```
