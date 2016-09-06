# vctrs

The goal of vctrs is to provide helpful functions for working with logical and numeric vectors, and atomic vectors in general. It's a natural complement to stringr (strings), lubridate (date/times), and forcats (factors). It provides three main types of functions:

* Summary
* Window (including rolling and cumulative)
* Vectorised

It has minimal dependencies and exposes both a C and R interface. This makes it easy to use from other packages like purrr and dplyr.

## Installation

You can install vctrs from github with:

``` r
# install.packages("devtools")
devtools::install_github("hadley/vctrs")
```
