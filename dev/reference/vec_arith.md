# Arithmetic operations

This generic provides a common double dispatch mechanism for all infix
operators (`+`, `-`, `/`, `*`, `^`, `%%`, `%/%`, `!`, `&`, `|`). It is
used to power the default arithmetic and boolean operators for
[vctr](https://vctrs.r-lib.org/dev/reference/new_vctr.md)s objects,
overcoming the limitations of the base
[Ops](https://rdrr.io/r/base/groupGeneric.html) generic.

## Usage

``` r
vec_arith(op, x, y, ...)

# Default S3 method
vec_arith(op, x, y, ...)

# S3 method for class 'logical'
vec_arith(op, x, y, ...)

# S3 method for class 'numeric'
vec_arith(op, x, y, ...)

vec_arith_base(op, x, y)

MISSING()
```

## Arguments

- op:

  An arithmetic operator as a string

- x, y:

  A pair of vectors. For `!`, unary `+` and unary `-`, `y` will be a
  sentinel object of class `MISSING`, as created by `MISSING()`.

- ...:

  These dots are for future extensions and must be empty.

## Details

`vec_arith_base()` is provided as a convenience for writing methods. It
recycles `x` and `y` to common length then calls the base operator with
the underlying
[`vec_data()`](https://vctrs.r-lib.org/dev/reference/vec_data.md).

`vec_arith()` is also used in `diff.vctrs_vctr()` method via `-`.

## See also

[`stop_incompatible_op()`](https://vctrs.r-lib.org/dev/reference/vctrs-conditions.md)
for signalling that an arithmetic operation is not permitted/supported.

See [`vec_math()`](https://vctrs.r-lib.org/dev/reference/vec_math.md)
for the equivalent for the unary mathematical functions.

## Examples

``` r
d <- as.Date("2018-01-01")
dt <- as.POSIXct("2018-01-02 12:00")
t <- as.difftime(12, unit = "hours")

vec_arith("-", dt, 1)
#> [1] "2018-01-02 11:59:59 UTC"
vec_arith("-", dt, t)
#> [1] "2018-01-02 UTC"
vec_arith("-", dt, d)
#> Time difference of 129600 secs

vec_arith("+", dt, 86400)
#> [1] "2018-01-03 12:00:00 UTC"
vec_arith("+", dt, t)
#> [1] "2018-01-03 UTC"
vec_arith("+", t, t)
#> Time difference of 24 hours

vec_arith("/", t, t)
#> [1] 1
vec_arith("/", t, 2)
#> Time difference of 6 hours

vec_arith("*", t, 2)
#> Time difference of 24 hours
```
