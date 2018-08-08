
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vctrs

[![Travis build
status](https://travis-ci.org/r-lib/vctrs.svg?branch=master)](https://travis-ci.org/r-lib/vctrs)
[![Coverage
status](https://codecov.io/gh/r-lib/vctrs/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/vctrs?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The short-term goal of vctrs is to develop a [type
system](https://en.wikipedia.org/wiki/Type_system) for vectors which
will help reason about functions that combine different types of input
(e.g. `c()`, `ifelse()`, `rbind()`). The vctrs type system encompasses
base vectors (e.g. logical, numeric, character, list), S3 vectors
(e.g. factor, ordered, Date, POSIXct), and data frames; and can be
extended to deal with S3 vectors defined in other packages, as described
in `vignette("extending-vctrs")`.

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
`ggplot2::cut_number()`, `dplyr::coalesce()`, and `tidyr::fill()`.

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

# combing dates and date-times give incorrect values
dt <- as.Date("2020-01-1")
dttm <- as.POSIXct(dt)

c(dt, dttm)
#> [1] "2020-01-01"    "4321940-06-07"
c(dttm, dt)
#> [1] "2019-12-31 18:00:00 CST" "1969-12-31 23:04:22 CST"

# as do combining dates and factors: factors
c(dt, factor("a"))
#> [1] "2020-01-01" "1970-01-02"
c(factor("a"), dt)
#> [1]     1 18262
```

This behaviour arises partly because `c()` has dual purposes: as well as
it’s primary duty of combining vectors, it has a secondary duty of
stripping attributes. For example, `?POSIXct` suggests that you should
use `c()` if you want to reset the timezone.

The second problem is that `dplyr::bind_rows()` is not extensible by
others. At the moment it handles S3 classes using a set of heuristics,
but these often fail, and it feels like we really need to think through
the problem in order to build a principled solution. This intersects
with the need to cleanly support more types of data frame columns
including lists of data frames, data frames, and matrices.

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

Unlike `c()`, you can optionally specify the desired output class:

``` r
vec_c(1, 2, .type = integer())
#> [1] 1 2
vec_c(1, "x", .type = character())
#> [1] "1" "x"
vec_c(1, "x", .type = list())
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] "x"
```

This supports a much wider range of casts (more on that below) than the
automatic coercions, but it can still fail:

``` r
vec_c(Sys.Date(), .type = factor())
#> Error: Can't cast date to factor
```

### What is a type?

There are a number of 1d types that are built-in to base R and supported
by vctrs: logical, integer, double, character, factor/ordered, date
(`Date`), datetime (`POSIXct`), and time (`difftime`). You can get a
textual representation of the type of a vector with `vec_type()`:

``` r
vec_type(letters)
#> type: character
vec_type(1:50)
#> type: integer
vec_type(list(1, 2, 3))
#> type: list
```

Internally, we represent the type of a vector with a 0-length subset of
the vector; this allows us to use value and types interchangeably in
many cases, and we can construct type specifications using base
constructors (e.g, `double()`, `factor(levels = c("a", "b"))`).

### Coercion and casting

The vctrs type system is defined by two functions: `vec_type2()` and
`vec_cast()`. `vec_type2()` is used for implicit coercions: given two
types, it returns their common type, or an error stating that there’s no
common type.

``` r
vec_type2(integer(), double())
#> numeric(0)

# no common type
vec_type2(factor(), Sys.Date())
#> Error: No common type for factor and date
```

`vec_type2()` is associative and commutative, so if you have more than
two types, you can use `Reduce()` to find the common type:

``` r
types <- list(integer(), double(), logical())
Reduce(vec_type2, types)
#> numeric(0)
```

`vec_cast()` is used for explicit casts: given a value and a type, it
casts the value to the type or throws an error stating that the cast is
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
#> Error: Can't cast double to factor
```

The set of possible casts is a subset of possible automatic coercions.
The following diagram summarises both casts (arrows) and coercions
(circles) for all base types supported by vctrs:

![](man/figures/combined.png)

### Data frames

vctrs defines the type of a data frame as the type of each column
labelled with the name of the column:

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
only occur in one:

``` r
vec_type(vec_type2(df1, df2))
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
vec_type(x1)
#> type: list_of<integer>

# This type is enforced if you attempt to modify the vector
x1[[4]] <- c(FALSE, TRUE, FALSE)
x1[[4]]
#> [1] 0 1 0

x1[[5]] <- factor("x")
#> Error: Can't cast factor to integer
```

This provides a natural type for nested data frames:

``` r
vec_type(as_list_of(split(mtcars, mtcars$cyl)))
#> type: list_of<data.frame<
#>  mpg : double
#>  cyl : double
#>  disp: double
#>  hp  : double
#>  drat: double
#>  wt  : double
#>  qsec: double
#>  vs  : double
#>  am  : double
#>  gear: double
#>  carb: double
#> >>
```

## Compared to base R

The following section compares base R and vctrs behavour. Changing base
R to follow the same set of principles as vctrs would lead to widespread
breakage in existing code, especially given that `c()` is often used for
its side-effect of stripping attributes (e.g. `?POSIXct` suggests using
`c(x)` to strip the time zone attribute).

### Atomic vectors

``` r
# c() will coerce any atomic type to character
c(1, "x")
#> [1] "1" "x"

# vctrs is stricter, and only casts explicitly
vec_c(1, "x")
#> Error: No common type for double and character

vec_c(1, "x", .type = character())
#> [1] "1" "x"
```

### Factors

``` r
fa <- factor("a")
fb <- factor("b")

# c() strips all factor attributes giving an integer vector
# (as documented in ?c)
c(fa, fb)
#> [1] 1 1

# unlist() creates a new factor with the union of the levels 
unlist(list(fa, fb))
#> [1] a b
#> Levels: a b

# vctrs always unions the levels
vec_c(fa, fb)
#> [1] a b
#> Levels: a b
```

### Dates and date-times

``` r
date <- as.Date("2020-01-01")
datetime <- as.POSIXct("2020-01-01 09:00")

# If the first argument to c() is a date, the result is a date
# But the datetime is not converted correctly (the number of seconds
# in the datetime is interpreted as the number of days in the date)
c(date, datetime)
#> [1] "2020-01-01"    "4322088-04-11"

# If the first argument to c() is a datetime, the result is a datetime
# But the date is not converted correctly (the number of days in the
# date is interpreted as the number of seconds in the date)
c(datetime, date)
#> [1] "2020-01-01 09:00:00 CST" "1969-12-31 23:04:22 CST"

# vctrs always returns the same type regardless of the order
# of the arguments, and converts dates to datetimes at midnight
vec_c(datetime, date)
#> [1] "2020-01-01 09:00:00 CST" "2020-01-01 00:00:00 CST"
vec_c(date, datetime)
#> [1] "2020-01-01 00:00:00 CST" "2020-01-01 09:00:00 CST"

# More subtly (as documented), c() drops the timezone, while
# vec_c() preserves it
datetime_nz <- as.POSIXct("2020-01-01 09:00", tz = "Pacific/Auckland")
c(datetime_nz, datetime_nz)
#> [1] "2019-12-31 14:00:00 CST" "2019-12-31 14:00:00 CST"
vec_c(datetime_nz, datetime_nz)
#> [1] "2020-01-01 09:00:00 NZDT" "2020-01-01 09:00:00 NZDT"
```

### Data frames

``` r
df1 <- data.frame(x = TRUE)
df2 <- data.frame(y = 2)

# rbind() requires the inputs to have identical column names
rbind(df1, df2)
#> Error in match.names(clabs, names(xi)): names do not match previous names

# vctrs creates a common type that is the union of the columns
vec_rbind(df1, df2)
#>      x  y
#> 1 TRUE NA
#> 2   NA  2

# Additionally, you can specify the desired output type
vec_rbind(df1, df2, .type = data.frame(x = double(), y = double()))
#>    x  y
#> 1  1 NA
#> 2 NA  2

# In some circumstances (combining data frames and vectors), 
# rbind() silently discards data
rbind(data.frame(x = 1:3), c(1, 1000000))
#>   x
#> 1 1
#> 2 2
#> 3 3
#> 4 1
```

### Tibbles

``` r
tb1 <- tibble::tibble(x = 3)

# rbind() uses the class of the first argument
class(rbind(tb1, df1))
#> [1] "tbl_df"     "tbl"        "data.frame"
class(rbind(df1, tb1))
#> [1] "data.frame"

# vctrs uses the common class of all arguments
class(vec_rbind(df1, df1))
#> [1] "data.frame"
class(vec_rbind(tb1, df1))
#> [1] "tbl_df"     "tbl"        "data.frame"
class(vec_rbind(df1, tb1))
#> [1] "tbl_df"     "tbl"        "data.frame"

# (this will help the tidyverse avoid turning data frames
# into tibbles when you're not using them)
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
