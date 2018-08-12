
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vctrs

[![Travis build
status](https://travis-ci.org/r-lib/vctrs.svg?branch=master)](https://travis-ci.org/r-lib/vctrs)
[![Coverage
status](https://codecov.io/gh/r-lib/vctrs/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/vctrs?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The short-term goal of vctrs specify the behavior of functions that
combine different types of vectors. This will help reason about
functions that combine different types of input (e.g. `c()`, `ifelse()`,
`rbind()`). The vctrs type system encompasses base vectors
(e.g. logical, numeric, character, list), S3 vectors (e.g. factor,
ordered, Date, POSIXct), and data frames; and can be extended to deal
with S3 vectors defined in other packages, as described in
`vignette("extending-vctrs")`.

Understanding and extending vctrs requires some effort from developers,
but it is our hope that the package will be invisible to most users.
Having an underlying theory that describes what type of thing a function
should return will mean that you can build up an accurate mental model
from day-to-day use, and you will be less surprised by new functions.

In the longer-term, vctrs will become the home for tidyverse vector
functions that work with logical and numeric vectors, and vectors in
general. This will make it a natural complement to
[stringr](https://stringr.tidyverse.org) (strings),
[lubridate](http://lubridate.tidyverse.org) (date/times), and
[forcats](https://forcats.tidyverse.org) (factors), and will bring
together various helpers that are currently scattered across packages,
`ggplot2::cut_number()`, `dplyr::coalesce()`, and `tidyr::fill()`. In
the very long-term, vctrs might provide the basis for a [type
system](https://en.wikipedia.org/wiki/Type_system) for vectors that
could help automate documentation and argument checking.

vctrs has few dependencies and is suitable for use from other packages.
(vctrs has a transitional dependency on tibble. Once vctrs is extensible
all tibble related code will move into the tibble package.)

## Installation

vctrs is not currently on CRAN. Install the development version from
GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("r-lib/vctrs")
```

## Motivation

The primary motivation comes from two separate, but related problems.
The first problem is that `base::c()` has rather undesirable behaviour
when you mix different S3 vectors:

``` r
# combining factors makes integers
c(factor("a"), factor("b"))
#> [1] 1 1

# even if you combine with a string
c("a", factor("a"))
#> [1] "a" "1"

# combing dates and date-times give incorrect values
dt <- as.Date("2020-01-1")
dttm <- as.POSIXct(dt)

c(dt, dttm)
#> [1] "2020-01-01"    "4321940-06-07"
c(dttm, dt)
#> [1] "2019-12-31 18:00:00 CST" "1969-12-31 23:04:22 CST"

# as do combining dates and factors
c(dt, factor("a"))
#> [1] "2020-01-01" "1970-01-02"
c(factor("a"), dt)
#> [1]     1 18262
```

This behaviour arises because `c()` has dual purposes: as well as it’s
primary duty of combining vectors, it has a secondary duty of stripping
attributes. For example, `?POSIXct` suggests that you should use `c()`
if you want to reset the timezone. A detailed comparison of vctrs vs
base R behaviour can be found in `vignettes("vctrs-vs-base.Rmd")`

The second problem is that `dplyr::bind_rows()` is not extensible by
others. Currently, it handles arbitrary S3 classes using heuristics, but
these often fail, and it feels like we really need to think through the
problem in order to build a principled solution. This intersects with
the need to cleanly support more types of data frame columns including
lists of data frames, data frames, and matrices.

## Usage

``` r
library(vctrs)
```

### Base vectors

`vec_c()` works like `c()`, but has stricter coercion rules:

``` r
vec_c(TRUE, 1)
#> [1] 1 1
vec_c(1L, 1.5)
#> [1] 1.0 1.5
vec_c(1.5, "x")
#> Error: No common type for double and character
```

Unlike `c()`, you can optionally specify the desired output class by
supplying a **prototype**, or ptype, for short:

``` r
vec_c(1, 2, .ptype = integer())
#> [1] 1 2
vec_c(1, "x", .ptype = character())
#> [1] "1" "x"
vec_c(1, "x", .ptype = list())
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] "x"
```

This supports a much wider range of casts (more on that below) than the
automatic coercions, but it can still fail:

``` r
vec_c(Sys.Date(), .ptype = factor())
#> Error: Can't cast date to factor<5152a>
```

### What is a prototype?

Internally, vctrs represents the class of a vector with a 0-length
subset. We call this a prototype, because it’s a miniature version of
the vector, that contains all of the attributes but none of the data.
Conveniently, you can create many prototypes using existing base
functions (e.g, `double()`, `factor(levels = c("a", "b"))`).

You can use `vec_ptype()` to create a prototype from an existing object.
It has a print method that summarises the prototype:

``` r
vec_ptype(letters)
#> prototype: character
vec_ptype(1:50)
#> prototype: integer
vec_ptype(list(1, 2, 3))
#> prototype: list
```

Some protoypes have parameters that affect their behaviour. These are
displayed where possible:

``` r
# Factors display a hash of their levels; this lets
# you distinguish different factors at a glance
vec_ptype(factor("a"))
#> prototype: factor<127a2>
vec_ptype(factor("b"))
#> prototype: factor<ddf10>

# Date-times display their timezone
vec_ptype(Sys.time())
#> prototype: datetime<local>

# difftimes display their units
vec_ptype(as.difftime(10, units = "mins"))
#> prototype: difftime<mins>
```

vctrs provides the `unknown()` class to represent vectors of unknown
type:

``` r
vec_ptype()
#> prototype: unknown
vec_ptype(NULL)
#> prototype: unknown

# NA is technically logical, but used in many places to
# represent a missing value of arbitrary type
vec_ptype(NA)
#> prototype: unknown
```

### Coercion and casting

vctrs defines the relationship between classes with two functions:
`vec_type2()` and `vec_cast()`. `vec_type2()` is used for implicit
coercions: given two classes, it returns the common class if it exists,
or otherwise throws and error. `vec_type2()` is commutative,
associative, and has an identity element, `unknown()`.

The easiest way to explore coercion is to give multiple arguments to
`vec_ptype()`. It uses `vec_type2()` to find the common type and
displays the results in a convenient form:

``` r
vec_ptype(integer(), double())
#> prototype: double
vec_ptype(Sys.Date(), Sys.time())
#> prototype: datetime<local>

# no common type
vec_ptype(factor(), Sys.Date())
#> Error: No common type for factor<5152a> and date
```

`vec_cast()` is used for explicit casts: given a value and a class, it
casts the value to the class or throws an error stating that the cast is
not possible. If a cast is possible in general (i.e. double -\>
integer), but information is lost for a specific input (e.g. 1.5 -\> 1),
it will generate a warning.

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
#> Error: Can't cast double to factor<127a2>
```

The set of possible casts is a subset of possible automatic coercions.
The following diagram summarises both casts (arrows) and coercions
(circles) for all base types supported by vctrs:

![](man/figures/combined.png)

### Factors

Note that the commutativity of `vec_type2()` only applies to the
prototype, not the attributes of the prototype. Concretely, the order in
which you concatenate factors will affect the order of the levels in the
output:

``` r
fa <- factor("a")
fb <- factor("b")

levels(vec_ptype(fa, fb)[[1]])
#> [1] "a" "b"
levels(vec_ptype(fb, fa)[[1]])
#> [1] "b" "a"
```

### Matrices and arrays

Any bare vector can have a `dim` attribute which turns it into a matrix
or array. The prototype of a matrix or array its a 0-row subset.

``` r
vec_ptype(array(1, c(1, 10)))
#> prototype: double[,10]
vec_ptype(array(1, c(1, 10, 10)))
#> prototype: double[,10,10]
```

A pair of arrays only has common type if the dimensions match:

``` r
vec_ptype(array(TRUE, c(2, 10)), array(1, c(5, 10)))
#> prototype: double[,10]

vec_ptype(array(TRUE, c(2, 10)), array(1, c(5, 1)))
#> Error: No common type for logical[,10] and double[,1]
#> Shapes are not compatible
vec_ptype(array(TRUE, c(2, 10)), array(1, c(5, 10, 1)))
#> Error: No common type for logical[,10] and double[,10,1]
#> Dimensionality must be equal
```

### Data frames

Data frames are defined by the names and prototypes of their columns:

``` r
df1 <- data.frame(x = TRUE, y = 1L)
vec_ptype(df1)
#> prototype: data.frame<
#>   x: logical
#>   y: integer
#> >

df2 <- data.frame(x = 1, z = 1)
vec_ptype(df2)
#> prototype: data.frame<
#>   x: double
#>   z: double
#> >
```

The common type of two data frames is the common type of each column
that occurs in both data frame frames, and the union of the columns that
only occur in one:

``` r
vec_ptype(df1, df2)
#> prototype: data.frame<
#>   x: double
#>   y: integer
#>   z: double
#> >
```

Like factors, the order of variables in the data frame is not
commutative, and depends on the order of the inputs:

``` r
vec_ptype(df1, df2)
#> prototype: data.frame<
#>   x: double
#>   y: integer
#>   z: double
#> >
vec_ptype(df2, df1)
#> prototype: data.frame<
#>   x: double
#>   z: double
#>   y: integer
#> >
```

Data frames are interesting because they are recursive: a data frame can
have a column that is also a data frame. vctrs knows how to handle these
too:

``` r
df3 <- data.frame(x = 2L)
df3$a <- data.frame(a = 2, b = 2)
vec_ptype(df3)
#> prototype: data.frame<
#>   x: integer
#>   a: 
#>     data.frame<
#>       a: double
#>       b: double
#>     >
#> >

df4 <- data.frame(x = 4)
df4$a <- data.frame(a = FALSE, b = 3, c = "a")
vec_ptype(df4)
#> prototype: data.frame<
#>   x: double
#>   a: 
#>     data.frame<
#>       a: logical
#>       b: double
#>       c: factor<127a2>
#>     >
#> >

vec_ptype(df3, df4)
#> prototype: data.frame<
#>   x: double
#>   a: 
#>     data.frame<
#>       a: double
#>       b: double
#>       c: factor<127a2>
#>     >
#> >
```

### List of

vctrs provides a new class that represents a list of elements with
constant prototype but varying lengths. This is an interesting contrast
to a data frame which is a list of elements with constant length, but
varying prototypes.

``` r
x1 <- list_of(1:3, 3:5, 6:8)
vec_ptype(x1)
#> prototype: list_of<integer>

# This type is enforced if you attempt to modify the vector
x1[[4]] <- c(FALSE, TRUE, FALSE)
x1[[4]]
#> [1] 0 1 0

x1[[5]] <- factor("x")
#> Error: Can't cast factor<5a425> to integer
```

This provides a natural type for nested data frames:

``` r
by_cyl <- data.frame(cyl = c(4, 6, 8))
by_cyl$data <- as_list_of(split(mtcars[1:3], mtcars$cyl))

vec_ptype(by_cyl)
#> prototype: data.frame<
#>   cyl : double
#>   data: 
#>     list_of<
#>       data.frame<
#>         mpg : double
#>         cyl : double
#>         disp: double
#>       >
#>     >
#> >
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
