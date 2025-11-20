# Find the prototype of a set of vectors

- `vec_ptype()` returns the
  [unfinalised](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
  prototype of a single vector.

- `vec_ptype_common()` returns the common type of multiple vectors. By
  default, this is
  [finalised](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
  for immediate usage, but can optionally be left unfinalised for
  advanced common type determination.

- `vec_ptype_show()` nicely prints the common type of any number of
  inputs, and is designed for interactive exploration.

## Usage

``` r
vec_ptype(x, ..., x_arg = "", call = caller_env())

vec_ptype_common(
  ...,
  .ptype = NULL,
  .finalise = TRUE,
  .arg = "",
  .call = caller_env()
)

vec_ptype_show(...)
```

## Arguments

- x:

  A vector

- ...:

  For `vec_ptype()`, these dots are for future extensions and must be
  empty.

  For `vec_ptype_common()` and `vec_ptype_show()`, vector inputs.

- x_arg:

  Argument name for `x`. This is used in error messages to inform the
  user about the locations of incompatible types.

- call, .call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- .ptype:

  If `NULL`, the default, the output type is determined by computing the
  common type across all elements of `...`.

  Alternatively, you can supply `.ptype` to give the output known type.
  If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this
  value: this is a convenient way to make production code demand fixed
  types.

- .finalise:

  Should `vec_ptype_common()`
  [finalise](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
  its output?

  - If `TRUE`,
    [`vec_ptype_finalise()`](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
    is called on the final `ptype` before it is returned. Practically
    this has the effect of converting any types from
    [unspecified](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
    to logical.

  - If `FALSE`,
    [unspecified](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
    types are left unfinalised, which can be useful for advanced cases
    where you combine one common type result with another type via
    [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md).
    Note that you must manually call
    [`vec_ptype_finalise()`](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
    on the final `ptype` before supplying it to any other vctrs
    functions.

- .arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

## Value

`vec_ptype()` and `vec_ptype_common()` return a prototype (a size-0
vector).

## `vec_ptype()`

`vec_ptype()` returns
[size](https://vctrs.r-lib.org/dev/reference/vec_size.md) 0 vectors
potentially containing attributes but no data. Generally, this is just
`vec_slice(x, 0L)`, but some inputs require special handling.

- While you can't slice `NULL`, the prototype of `NULL` is itself. This
  is because we treat `NULL` as an identity value in the
  [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
  monoid.

- The prototype of logical vectors that only contain missing values is
  the special
  [unspecified](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
  type, which can be coerced to any other 1d type. This allows bare
  `NA`s to represent missing values for any 1d vector type.
  [Finalising](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
  this type converts it from unspecified back to logical.

See
[internal-faq-ptype2-identity](https://vctrs.r-lib.org/dev/reference/internal-faq-ptype2-identity.md)
for more information about identity values.

`vec_ptype()` is a *performance* generic. It is not necessary to
implement it because the default method will work for any vctrs type.
However the default method builds around other vctrs primitives like
[`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)
which incurs performance costs. If your class has a static prototype,
you might consider implementing a custom `vec_ptype()` method that
returns a constant. This will improve the performance of your class in
many cases ([common
type](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md) imputation in
particular).

Because it may contain unspecified vectors, the prototype returned by
`vec_ptype()` is said to be **unfinalised**. Call
[`vec_ptype_finalise()`](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
to finalise it.

## `vec_ptype_common()`

`vec_ptype_common()` first finds the prototype of each input, then
successively calls
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md) to
find a common type. It returns a
[finalised](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)
prototype by default, but can optionally be left unfinalised for
advanced common type determination.

## Dependencies of `vec_ptype()`

- [`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)
  for returning an empty slice

## Dependencies of `vec_ptype_common()`

- [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)

- [`vec_ptype_finalise()`](https://vctrs.r-lib.org/dev/reference/vctrs-unspecified.md)

## Examples

``` r
# Unknown types ------------------------------------------
vec_ptype_show()
#> Prototype: NULL
vec_ptype_show(NULL)
#> Prototype: NULL

# Vectors ------------------------------------------------
vec_ptype_show(1:10)
#> Prototype: integer
vec_ptype_show(letters)
#> Prototype: character
vec_ptype_show(TRUE)
#> Prototype: logical

vec_ptype_show(Sys.Date())
#> Prototype: date
vec_ptype_show(Sys.time())
#> Prototype: datetime<local>
vec_ptype_show(factor("a"))
#> Prototype: factor<4d52a>
vec_ptype_show(ordered("a"))
#> Prototype: ordered<4d52a>

# Matrices -----------------------------------------------
# The prototype of a matrix includes the number of columns
vec_ptype_show(array(1, dim = c(1, 2)))
#> Prototype: double[,2]
vec_ptype_show(array("x", dim = c(1, 2)))
#> Prototype: character[,2]

# Data frames --------------------------------------------
# The prototype of a data frame includes the prototype of
# every column
vec_ptype_show(iris)
#> Prototype: data.frame<
#>   Sepal.Length: double
#>   Sepal.Width : double
#>   Petal.Length: double
#>   Petal.Width : double
#>   Species     : factor<fb977>
#> >

# The prototype of multiple data frames includes the prototype
# of every column that in any data frame
vec_ptype_show(
  data.frame(x = TRUE),
  data.frame(y = 2),
  data.frame(z = "a")
)
#> Prototype: <data.frame<
#>   x: logical
#>   y: double
#>   z: character
#> >>
#> 0. (                         , <data.frame<x:logical>>   ) = <data.frame<x:logical>>
#> 1. ┌ <data.frame<x:logical>> , <data.frame<y:double>>    ┐ = <data.frame<           
#>    │                                                     │     x: logical           
#>    │                                                     │     y: double            
#>    └                                                     ┘   >>                     
#> 2. ┌ <data.frame<            , <data.frame<z:character>> ┐ = <data.frame<           
#>    │   x: logical                                        │     x: logical           
#>    │   y: double                                         │     y: double            
#>    │ >>                                                  │     z: character         
#>    └                                                     ┘   >>                     

# Finalisation -------------------------------------------

# `vec_ptype()` and `vec_ptype2()` return unfinalised ptypes so that they
# can be coerced to any other type
vec_ptype(NA)
#> <unspecified> [0]
vec_ptype2(NA, NA)
#> <unspecified> [0]

# By default `vec_ptype_common()` finalises so that you can use its result
# directly in other vctrs functions
vec_ptype_common(NA, NA)
#> logical(0)

# You can opt out of finalisation to make it work like `vec_ptype()` and
# `vec_ptype2()` with `.finalise = FALSE`, but don't forget that you must
# call `vec_ptype_finalise()` manually if you do so!
vec_ptype_common(NA, NA, .finalise = FALSE)
#> <unspecified> [0]
vec_ptype_finalise(vec_ptype_common(NA, NA, .finalise = FALSE))
#> logical(0)

# This can be useful in rare scenarios, like including a separate `default`
# argument in the ptype computation
xs <- list(NA, NA)
default <- "a"
try(vec_ptype2(vec_ptype_common(!!!xs), default))
#> Error in eval(expr, envir) : 
#>   Can't combine `vec_ptype_common(!!!xs)` <logical> and `default` <character>.
vec_ptype2(vec_ptype_common(!!!xs, .finalise = FALSE), default)
#> character(0)
```
