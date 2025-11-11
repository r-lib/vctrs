# `print()` and `str()` generics.

These are constructed to be more easily extensible since you can
override the `_header()`, `_data()` or `_footer()` components
individually. The default methods are built on top of
[`format()`](https://rdrr.io/r/base/format.html).

## Usage

``` r
obj_print(x, ...)

obj_print_header(x, ...)

obj_print_data(x, ...)

obj_print_footer(x, ...)

obj_str(x, ...)

obj_str_header(x, ...)

obj_str_data(x, ...)

obj_str_footer(x, ...)
```

## Arguments

- x:

  A vector

- ...:

  Additional arguments passed on to methods. See
  [`print()`](https://rdrr.io/r/base/print.html) and
  [`str()`](https://rdrr.io/r/utils/str.html) for commonly used options
