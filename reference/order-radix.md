# Order and sort vectors

`vec_order_radix()` computes the order of `x`. For data frames, the
order is computed along the rows by computing the order of the first
column and using subsequent columns to break ties.

`vec_sort_radix()` sorts `x`. It is equivalent to
`vec_slice(x, vec_order_radix(x))`.

## Usage

``` r
vec_order_radix(
  x,
  ...,
  direction = "asc",
  na_value = "largest",
  nan_distinct = FALSE,
  chr_proxy_collate = NULL
)

vec_sort_radix(
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

- `vec_order_radix()` an integer vector the same size as `x`.

- `vec_sort_radix()` a vector with the same size and type as `x`.

## Differences with [`order()`](https://rdrr.io/r/base/order.html)

Unlike the `na.last` argument of
[`order()`](https://rdrr.io/r/base/order.html) which decides the
positions of missing values irrespective of the `decreasing` argument,
the `na_value` argument of `vec_order_radix()` interacts with
`direction`. If missing values are considered the largest value, they
will appear last in ascending order, and first in descending order.

Character vectors are ordered in the C-locale. This is different from
[`base::order()`](https://rdrr.io/r/base/order.html), which respects
[`base::Sys.setlocale()`](https://rdrr.io/r/base/locales.html). Sorting
in a consistent locale can produce more reproducible results between
different sessions and platforms, however, the results of sorting in the
C-locale can be surprising. For example, capital letters sort before
lower case letters. Sorting `c("b", "C", "a")` with `vec_sort_radix()`
will return `c("C", "a", "b")`, but with
[`base::order()`](https://rdrr.io/r/base/order.html) will return
`c("a", "b", "C")` unless `base::order(method = "radix")` is explicitly
set, which also uses the C-locale. While sorting with the C-locale can
be useful for algorithmic efficiency, in many real world uses it can be
the cause of data analysis mistakes. To balance these trade-offs, you
can supply a `chr_proxy_collate` function to transform character vectors
into an alternative representation that orders in the C-locale in a less
surprising way. For example, providing
[`base::tolower()`](https://rdrr.io/r/base/chartr.html) as a transform
will order the original vector in a case-insensitive manner.
Locale-aware ordering can be achieved by providing
[`stringi::stri_sort_key()`](https://rdrr.io/pkg/stringi/man/stri_sort_key.html)
as a transform, setting the collation options as appropriate for your
locale.

Character vectors are always translated to UTF-8 before ordering, and
before any transform is applied by `chr_proxy_collate`.

For complex vectors, if either the real or imaginary component is `NA`
or `NaN`, then the entire observation is considered missing.

## Dependencies of `vec_order_radix()`

- [`vec_proxy_order()`](https://vctrs.r-lib.org/reference/vec_proxy_compare.md)

## Dependencies of `vec_sort_radix()`

- `vec_order_radix()`

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

## Examples

``` r
if (FALSE) {

x <- round(sample(runif(5), 9, replace = TRUE), 3)
x <- c(x, NA)

vec_order_radix(x)
vec_sort_radix(x)
vec_sort_radix(x, direction = "desc")

# Can also handle data frames
df <- data.frame(g = sample(2, 10, replace = TRUE), x = x)
vec_order_radix(df)
vec_sort_radix(df)
vec_sort_radix(df, direction = "desc")

# For data frames, `direction` and `na_value` are allowed to be vectors
# with length equal to the number of columns in the data frame
vec_sort_radix(
  df,
  direction = c("desc", "asc"),
  na_value = c("largest", "smallest")
)

# Character vectors are ordered in the C locale, which orders capital letters
# below lowercase ones
y <- c("B", "A", "a")
vec_sort_radix(y)

# To order in a case-insensitive manner, provide a `chr_proxy_collate`
# function that transforms the strings to all lowercase
vec_sort_radix(y, chr_proxy_collate = tolower)

}
```
