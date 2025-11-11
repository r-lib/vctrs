# vctr (vector) S3 class

This abstract class provides a set of useful default methods that makes
it considerably easier to get started with a new S3 vector class. See
[`vignette("s3-vector")`](https://vctrs.r-lib.org/dev/articles/s3-vector.md)
to learn how to use it to create your own S3 vector classes.

## Usage

``` r
new_vctr(.data, ..., class = character(), inherit_base_type = NULL)
```

## Arguments

- .data:

  Foundation of class. Must be a vector

- ...:

  Name-value pairs defining attributes

- class:

  Name of subclass.

- inherit_base_type:

  **\[experimental\]** A single logical, or `NULL`. Does this class
  extend the base type of `.data`? i.e. does the resulting object extend
  the behaviour of the underlying type? Defaults to `FALSE` for all
  types except lists, which are required to inherit from the base type.

## Details

List vctrs are special cases. When created through `new_vctr()`, the
resulting list vctr should always be recognized as a list by
[`obj_is_list()`](https://vctrs.r-lib.org/dev/reference/obj_is_list.md).
Because of this, if `inherit_base_type` is `FALSE` an error is thrown.

## Base methods

The vctr class provides methods for many base generics using a smaller
set of generics defined by this package. Generally, you should think
carefully before overriding any of the methods that vctrs implements for
you as they've been carefully planned to be internally consistent.

- `[[` and `[` use
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) dispatch to
  the underlying base function, then restore attributes with
  [`vec_restore()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md).
  [`rep()`](https://rdrr.io/r/base/rep.html) and `length<-` work
  similarly.

- `[[<-` and `[<-` cast `value` to same type as `x`, then call
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html).

- [`as.logical()`](https://rdrr.io/r/base/logical.html),
  [`as.integer()`](https://rdrr.io/r/base/integer.html),
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html),
  [`as.character()`](https://rdrr.io/r/base/character.html),
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html) and
  [`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) methods call
  [`vec_cast()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md). The
  [`as.list()`](https://rdrr.io/r/base/list.html) method calls `[[`
  repeatedly, and the
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method
  uses a standard technique to wrap a vector in a data frame.

- [`as.factor()`](https://rdrr.io/r/base/factor.html),
  [`as.ordered()`](https://rdrr.io/r/base/factor.html) and
  [`as.difftime()`](https://rdrr.io/r/base/difftime.html) are not
  generic functions in base R, but have been reimplemented as generics
  in the `generics` package. `vctrs` extends these and calls
  [`vec_cast()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md). To
  inherit this behaviour in a package, import and re-export the generic
  of interest from `generics`.

- `==`, `!=`, [`unique()`](https://rdrr.io/r/base/unique.html),
  [`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html), and
  [`is.na()`](https://rdrr.io/r/base/NA.html) use
  [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md).

- `<`, `<=`, `>=`, `>`, [`min()`](https://rdrr.io/r/base/Extremes.html),
  [`max()`](https://rdrr.io/r/base/Extremes.html),
  [`range()`](https://rdrr.io/r/base/range.html),
  [`median()`](https://rdrr.io/r/stats/median.html),
  [`quantile()`](https://rdrr.io/r/stats/quantile.html), and
  [`xtfrm()`](https://rdrr.io/r/base/xtfrm.html) methods use
  [`vec_proxy_compare()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_compare.md).

- `+`, `-`, `/`, `*`, `^`, `%%`, `%/%`, `!`, `&`, and `|` operators use
  [`vec_arith()`](https://vctrs.r-lib.org/dev/reference/vec_arith.md).

- Mathematical operations including the Summary group generics
  ([`prod()`](https://rdrr.io/r/base/prod.html),
  [`sum()`](https://rdrr.io/r/base/sum.html),
  [`any()`](https://rdrr.io/r/base/any.html),
  [`all()`](https://rdrr.io/r/base/all.html)), the Math group generics
  ([`abs()`](https://rdrr.io/r/base/MathFun.html),
  [`sign()`](https://rdrr.io/r/base/sign.html), etc),
  [`mean()`](https://rdrr.io/r/base/mean.html),
  [`is.nan()`](https://rdrr.io/r/base/is.finite.html),
  [`is.finite()`](https://rdrr.io/r/base/is.finite.html), and
  [`is.infinite()`](https://rdrr.io/r/base/is.finite.html) use
  [`vec_math()`](https://vctrs.r-lib.org/dev/reference/vec_math.md).

- `dims()`, `dims<-`,
  [`dimnames()`](https://rdrr.io/r/base/dimnames.html), `dimnames<-`,
  [`levels()`](https://rdrr.io/r/base/levels.html), and `levels<-`
  methods throw errors.
