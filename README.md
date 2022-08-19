
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vctrs <img src="man/figures/logo.png" align="right" width=120 height=139 alt="" />

<!-- badges: start -->

[![Coverage
status](https://codecov.io/gh/r-lib/vctrs/branch/main/graph/badge.svg)](https://codecov.io/github/r-lib/vctrs?branch=master)
![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
[![R-CMD-check](https://github.com/r-lib/vctrs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-lib/vctrs/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

There are three main goals to the vctrs package, each described in a
vignette:

-   To propose `vec_size()` and `vec_ptype()` as alternatives to
    `length()` and `class()`; `vignette("type-size")`. These definitions
    are paired with a framework for size-recycling and type-coercion.
    `ptype` should evoke the notion of a prototype, i.e. the original or
    typical form of something.

-   To define size- and type-stability as desirable function properties,
    use them to analyse existing base functions, and to propose better
    alternatives; `vignette("stability")`. This work has been
    particularly motivated by thinking about the ideal properties of
    `c()`, `ifelse()`, and `rbind()`.

-   To provide a new `vctr` base class that makes it easy to create new
    S3 vectors; `vignette("s3-vector")`. vctrs provides methods for many
    base generics in terms of a few new vctrs generics, making
    implementation considerably simpler and more robust.

vctrs is a developer-focussed package. Understanding and extending vctrs
requires some effort from developers, but should be invisible to most
users. It’s our hope that having an underlying theory will mean that
users can build up an accurate mental model without explicitly learning
the theory. vctrs will typically be used by other packages, making it
easy for them to provide new classes of S3 vectors that are supported
throughout the tidyverse (and beyond). For that reason, vctrs has few
dependencies.

## Installation

Install vctrs from CRAN with:

``` r
install.packages("vctrs")
```

Alternatively, if you need the development version, install it with:

``` r
# install.packages("devtools")
devtools::install_github("r-lib/vctrs")
```

## Usage

``` r
library(vctrs)

# Sizes
str(vec_size_common(1, 1:10))
#>  int 10
str(vec_recycle_common(1, 1:10))
#> List of 2
#>  $ : num [1:10] 1 1 1 1 1 1 1 1 1 1
#>  $ : int [1:10] 1 2 3 4 5 6 7 8 9 10

# Prototypes
str(vec_ptype_common(FALSE, 1L, 2.5))
#>  num(0)
str(vec_cast_common(FALSE, 1L, 2.5))
#> List of 3
#>  $ : num 0
#>  $ : num 1
#>  $ : num 2.5
```

## Motivation

The original motivation for vctrs comes from two separate but related
problems. The first problem is that `base::c()` has rather undesirable
behaviour when you mix different S3 vectors:

``` r
# combining factors makes integers
c(factor("a"), factor("b"))
#> [1] 1 1

# combining dates and date-times gives incorrect values; also, order matters
dt <- as.Date("2020-01-01")
dttm <- as.POSIXct(dt)

c(dt, dttm)
#> [1] "2020-01-01"    "4321940-06-07"
c(dttm, dt)
#> [1] "2019-12-31 19:00:00 EST" "1970-01-01 00:04:22 EST"
```

This behaviour arises because `c()` has dual purposes: as well as its
primary duty of combining vectors, it has a secondary duty of stripping
attributes. For example, `?POSIXct` suggests that you should use `c()`
if you want to reset the timezone.

The second problem is that `dplyr::bind_rows()` is not extensible by
others. Currently, it handles arbitrary S3 classes using heuristics, but
these often fail, and it feels like we really need to think through the
problem in order to build a principled solution. This intersects with
the need to cleanly support more types of data frame columns, including
lists of data frames, data frames, and matrices.
