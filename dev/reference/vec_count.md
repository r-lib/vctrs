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
#> 1    f    21
#> 2    e    19
#> 3    g    16
#> 4    d    14
#> 5    h     9
#> 6    b     6
#> 7    c     4
#> 8    j     3
#> 9    k     2
#> 10   l     2
#> 11   i     2
#> 12   n     1
#> 13   a     1

# by can sort by key
vec_count(x, sort = "key")
#>    key count
#> 1    a     1
#> 2    b     6
#> 3    c     4
#> 4    d    14
#> 5    e    19
#> 6    f    21
#> 7    g    16
#> 8    h     9
#> 9    i     2
#> 10   j     3
#> 11   k     2
#> 12   l     2
#> 13   n     1

# or location of first value
vec_count(x, sort = "location")
#>    key count
#> 1    e    19
#> 2    d    14
#> 3    c     4
#> 4    k     2
#> 5    g    16
#> 6    l     2
#> 7    b     6
#> 8    f    21
#> 9    j     3
#> 10   h     9
#> 11   i     2
#> 12   n     1
#> 13   a     1
head(x)
#> [1] "e" "e" "d" "e" "c" "e"

# or not at all
vec_count(x, sort = "none")
#>    key count
#> 1    n     1
#> 2    k     2
#> 3    l     2
#> 4    e    19
#> 5    h     9
#> 6    f    21
#> 7    j     3
#> 8    g    16
#> 9    b     6
#> 10   i     2
#> 11   c     4
#> 12   a     1
#> 13   d    14
```
