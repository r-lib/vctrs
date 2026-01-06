# Count unique values in a vector

Count the number of unique values in a vector. `vec_count()` has two
important differences to
[`table()`](https://vctrs.r-lib.org/dev/reference/table.md): it returns
a data frame, and when given multiple inputs (as a data frame), it only
counts combinations that appear in the input.

## Usage

``` r
vec_count(x, sort = c("count", "key", "location", "none"))
```

## Arguments

- x:

  A vector (including a data frame).

- sort:

  One of "count", "key", "location", or "none".

  - "count", the default, puts most frequent values at top

  - "key", orders by the output key column (i.e. unique values of `x`)

  - "location", orders by location where key first seen. This is useful
    if you want to match the counts up to other unique/duplicated
    functions.

  - "none", leaves unordered. This is not guaranteed to produce the same
    ordering across R sessions, but is the fastest method.

## Value

A data frame with columns `key` (same type as `x`) and `count` (an
integer vector).

## Dependencies

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md)

- [`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)

- [`vec_order()`](https://vctrs.r-lib.org/dev/reference/vec_order.md)

## Examples

``` r
vec_count(mtcars$vs)
#>   key count
#> 1   0    18
#> 2   1    14
vec_count(iris$Species)
#>          key count
#> 1     setosa    50
#> 2 versicolor    50
#> 3  virginica    50

# If you count a data frame you'll get a data frame
# column in the output
str(vec_count(mtcars[c("vs", "am")]))
#> 'data.frame':    4 obs. of  2 variables:
#>  $ key  :'data.frame':   4 obs. of  2 variables:
#>   ..$ vs: num  0 1 1 0
#>   ..$ am: num  0 1 0 1
#>  $ count: int  12 7 7 6

# Sorting ---------------------------------------

x <- letters[rpois(100, 6)]
# default is to sort by frequency
vec_count(x)
#>    key count
#> 1    f    16
#> 2    g    16
#> 3    d    15
#> 4    e    11
#> 5    h    11
#> 6    i    11
#> 7    j     5
#> 8    b     4
#> 9    c     4
#> 10   n     2
#> 11   a     2
#> 12   m     2
#> 13   k     1

# by can sort by key
vec_count(x, sort = "key")
#>    key count
#> 1    a     2
#> 2    b     4
#> 3    c     4
#> 4    d    15
#> 5    e    11
#> 6    f    16
#> 7    g    16
#> 8    h    11
#> 9    i    11
#> 10   j     5
#> 11   k     1
#> 12   m     2
#> 13   n     2

# or location of first value
vec_count(x, sort = "location")
#>    key count
#> 1    e    11
#> 2    f    16
#> 3    j     5
#> 4    d    15
#> 5    g    16
#> 6    b     4
#> 7    n     2
#> 8    c     4
#> 9    h    11
#> 10   i    11
#> 11   a     2
#> 12   m     2
#> 13   k     1
head(x)
#> [1] "e" "f" "j" "e" "d" "g"

# or not at all
vec_count(x, sort = "none")
#>    key count
#> 1    c     4
#> 2    g    16
#> 3    e    11
#> 4    k     1
#> 5    b     4
#> 6    n     2
#> 7    h    11
#> 8    i    11
#> 9    m     2
#> 10   j     5
#> 11   d    15
#> 12   f    16
#> 13   a     2
```
