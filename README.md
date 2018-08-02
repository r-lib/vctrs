
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vctrs

[![Travis build
status](https://travis-ci.org/r-lib/vctrs.svg?branch=master)](https://travis-ci.org/r-lib/vctrs)
[![Coverage
status](https://codecov.io/gh/r-lib/vctrs/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/vctrs?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The primary short-term goal of vctrs is to develop a theory of “types”
that help us reason about the “correct” type and shape to return from
vectorised functions that have to combine inputs with different types
(e.g. `c()` and `dplyr::bind_rows()`). For the user, the goal is to be
invisible: a principled approach to type coercion will provide greater
consistency across functions, leading to a more accurate mental model
and fewer suprises.

In the medium-term, we will provide developer documentation for creating
new types of S3 vector. This will describe what you need to do make your
new class by into the vctrs coercion philosophy, well as what base
generics you should be supply methods for. We will also reimplement the
core coercions in C for performance, and consider how best to perform
double dispatch without using S4.

In the long-run, vctrs will also expand to provide functions that
working with logical and numeric vectors, and vectors in general. It
will become a natural complement to stringr (strings), lubridate
(date/times), and forcats (factors), and bring together various helpers
that are currently scattered across packages like ggplot2 (e.g.
`cut_number()`), dplyr (e.g. `coalese()`), and tidyr (e.g. `fill()`).
vctrs is a low-dependency package suitable that will be used from other
packages like purrr and dplyr.

## Installation

You can install the development version of vctrs from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("r-lib/vctrs")
```

## Examples

### Base vectors

``` r
library(vctrs)

# vec_c() basically works like c(), but with stricter rules for what 
# gets coerced
vec_c(TRUE, 1)
#> [1] 1 1
vec_c(1L, 1.5)
#> [1] 1.0 1.5
vec_c(1.5, "x")
#> Error: No common type for double and character

# You can override the rules by supplying the desired type of the output
vec_c(1, "x", .type = character())
#> [1] "1" "x"
vec_c(1, "x", .type = list())
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] "x"

# Coercions will warn when lossy (i.e. you can't round trip)
vec_c(1, 1.5, 2, .type = integer())
#> Warning: Lossy conversion from double to integer [Locations: 1]
#> [1] 1 1 2
```

### What is a type?

``` r
# Describe the type of a vector with vec_type()
vec_type(letters)
#> type: character
vec_type(1:50)
#> type: integer
vec_type(list(1, 2, 3))
#> type: list
```

Internally, we represent the type of a vector with a 0-length subset of
the vector; that allows us to use value and types interchangeably in
many cases, and we can rely base constructors for empty vectors.

### Coercion and casting

There are two primary components to the implementation of vectors,
`vec_type2()` and `vec_cast()`.

`vec_type2()` is used for implicit coercions: given two types, it
returns their common type or dies trying. `vec_type2()` is associative
and commutative, which means that to find the common type for more than
two vectors, you can use `Reduce()`.

``` r
vec_type2(integer(), double())
#> numeric(0)

# no common type
vec_type2(factor(), Sys.Date())
#> Error: No common type for factor and date

types <- list(integer(), double(), logical())
Reduce(vec_type2, types)
#> numeric(0)
```

`vec_cast()` is used for explicit casts: given a value and a type, it
casts the value to the type, or dies trying. It will warn if a cast is
possible in general, but information is lost for a specific input.

``` r
# Cast succeeds
vec_cast(c(1, 2), integer())
#> [1] 1 2

# Cast loses information
vec_cast(c(1.5, 2.5), integer())
#> Warning: Lossy conversion from double to integer [Locations: 1, 2]
#> [1] 1 2

# Cast fails
vec_cast(c(1.5, 2.5), factor("a"))
#> Error: Can't cast double to factor
```

The set of possible casts is a subset of possible automatic coercions,
and both are summarised in the following diagram where arrows represent
automatic coercions, and circles represent possible casts.

![](man/figures/combined.png)

### Data frames

The type of a data frame is the type of each column:

``` r
df1 <- data.frame(x = TRUE, y = 1L)
vec_type(df1)
#> type: data.frame<
#>  x: logical
#>  y: integer
#> >

df2 <- data.frame(x = 1, z = 1)
vec_type(df2)
#> type: data.frame<
#>  x: double
#>  z: double
#> >
```

The common type of two data frames is the common type of each column
that occurs in both data frame frames, and the union of the columns that
are unique:

``` r
max(vec_type(df1), vec_type(df2))
#> type: data.frame<
#>  x: double
#>  y: integer
#>  z: double
#> >
```

### List of

vctr provides a new vector type that represents a list where each
element has the same type (an interesting contrast to a data frame which
is a list where each element has the same *length*).

``` r
x1 <- list_of(1:3, 3:5, 6:8)
vec_type(x)
#> Error in is_vector(x): object 'x' not found

# This type is enforced if you attempt to modify the vector
x1[[4]] <- c(FALSE, TRUE, FALSE)
x1[[4]]
#> [1] 0 1 0

x1[[5]] <- factor("x")
#> Error: Can't cast factor to integer
```

## Tidyverse functions

There are a number of tidyverse functions that currently need to do type
coercion. In the long run, their varied and idiosyncratic approaches
will be replaced by the systematic foundation provided by vctrs.

``` r
# Data frame functions
dplyr::inner_join() # and friends
dplyr::bind_rows()
dplyr::summarise()
dplyr::mutate()

tidyr::gather()
tidyr::unnest()

# Vector functions
purrr::flatten()
purrr::map_c()
purrr::transpose()

dplyr::combine()
dplyr::if_else()
dplyr::recode()
dplyr::case_when()
dplyr::coalesce()
dplyr::na_if()
```
