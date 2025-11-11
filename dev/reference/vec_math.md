# Mathematical operations

This generic provides a common dispatch mechanism for all regular unary
mathematical functions. It is used as a common wrapper around many of
the Summary group generics, the Math group generics, and a handful of
other mathematical functions like
[`mean()`](https://rdrr.io/r/base/mean.html) (but not
[`var()`](https://rdrr.io/r/stats/cor.html) or
[`sd()`](https://rdrr.io/r/stats/sd.html)).

## Usage

``` r
vec_math(.fn, .x, ...)

vec_math_base(.fn, .x, ...)
```

## Arguments

- .fn:

  A mathematical function from the base package, as a string.

- .x:

  A vector.

- ...:

  Additional arguments passed to `.fn`.

## Details

`vec_math_base()` is provided as a convenience for writing methods. It
calls the base `.fn` on the underlying
[`vec_data()`](https://vctrs.r-lib.org/dev/reference/vec_data.md).

## Included functions

- From the [Summary](https://rdrr.io/r/base/groupGeneric.html) group
  generic: [`prod()`](https://rdrr.io/r/base/prod.html),
  [`sum()`](https://rdrr.io/r/base/sum.html),
  [`any()`](https://rdrr.io/r/base/any.html),
  [`all()`](https://rdrr.io/r/base/all.html).

- From the [Math](https://rdrr.io/r/base/groupGeneric.html) group
  generic: [`abs()`](https://rdrr.io/r/base/MathFun.html),
  [`sign()`](https://rdrr.io/r/base/sign.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
  [`ceiling()`](https://rdrr.io/r/base/Round.html),
  [`floor()`](https://rdrr.io/r/base/Round.html),
  [`trunc()`](https://rdrr.io/r/base/Round.html),
  [`cummax()`](https://rdrr.io/r/base/cumsum.html),
  [`cummin()`](https://rdrr.io/r/base/cumsum.html),
  [`cumprod()`](https://rdrr.io/r/base/cumsum.html),
  [`cumsum()`](https://rdrr.io/r/base/cumsum.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`log10()`](https://rdrr.io/r/base/Log.html),
  [`log2()`](https://rdrr.io/r/base/Log.html),
  [`log1p()`](https://rdrr.io/r/base/Log.html),
  [`acos()`](https://rdrr.io/r/base/Trig.html),
  [`acosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`asin()`](https://rdrr.io/r/base/Trig.html),
  [`asinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`atan()`](https://rdrr.io/r/base/Trig.html),
  [`atanh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`exp()`](https://rdrr.io/r/base/Log.html),
  [`expm1()`](https://rdrr.io/r/base/Log.html),
  [`cos()`](https://rdrr.io/r/base/Trig.html),
  [`cosh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`cospi()`](https://rdrr.io/r/base/Trig.html),
  [`sin()`](https://rdrr.io/r/base/Trig.html),
  [`sinh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`sinpi()`](https://rdrr.io/r/base/Trig.html),
  [`tan()`](https://rdrr.io/r/base/Trig.html),
  [`tanh()`](https://rdrr.io/r/base/Hyperbolic.html),
  [`tanpi()`](https://rdrr.io/r/base/Trig.html),
  [`gamma()`](https://rdrr.io/r/base/Special.html),
  [`lgamma()`](https://rdrr.io/r/base/Special.html),
  [`digamma()`](https://rdrr.io/r/base/Special.html),
  [`trigamma()`](https://rdrr.io/r/base/Special.html).

- Additional generics: [`mean()`](https://rdrr.io/r/base/mean.html),
  [`is.nan()`](https://rdrr.io/r/base/is.finite.html),
  [`is.finite()`](https://rdrr.io/r/base/is.finite.html),
  [`is.infinite()`](https://rdrr.io/r/base/is.finite.html).

Note that [`median()`](https://rdrr.io/r/stats/median.html) is currently
not implemented, and [`sd()`](https://rdrr.io/r/stats/sd.html) and
[`var()`](https://rdrr.io/r/stats/cor.html) are currently not generic
and so do not support custom classes.

## See also

[`vec_arith()`](https://vctrs.r-lib.org/dev/reference/vec_arith.md) for
the equivalent for the arithmetic infix operators.

## Examples

``` r
x <- new_vctr(c(1, 2.5, 10))
x
#> <vctrs_vctr[3]>
#> [1]  1.0  2.5 10.0

abs(x)
#> <vctrs_vctr[3]>
#> [1]  1.0  2.5 10.0
sum(x)
#> <vctrs_vctr[1]>
#> [1] 13.5
cumsum(x)
#> <vctrs_vctr[3]>
#> [1]  1.0  3.5 13.5
```
