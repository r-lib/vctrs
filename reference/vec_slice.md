# Get or set observations in a vector

This provides a common interface to extracting and modifying
observations for all vector types, regardless of dimensionality. They
are analogs to `[` and `[<-` that match
[`vec_size()`](https://vctrs.r-lib.org/reference/vec_size.md) instead of
[`length()`](https://rdrr.io/r/base/length.html).

## Usage

``` r
vec_slice(x, i, ..., error_call = current_env())

vec_slice(x, i) <- value

vec_assign(x, i, value, ..., slice_value = FALSE, x_arg = "", value_arg = "")
```

## Arguments

- x:

  A vector

- i:

  An integer, character or logical vector specifying the locations or
  names of the observations to get/set. Specify `TRUE` to index all
  elements (as in `x[]`), or `NULL`, `FALSE` or
  [`integer()`](https://rdrr.io/r/base/integer.html) to index none (as
  in `x[NULL]`).

- ...:

  These dots are for future extensions and must be empty.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- value:

  A vector of replacement values

  `value` is cast to the type of `x`.

  If `slice_value = FALSE`, `value` must be size 1 or the same size as
  `i` after `i` has been converted to a positive integer location vector
  with
  [`vec_as_location()`](https://vctrs.r-lib.org/reference/vec_as_location.md)
  (which may not be the same size as `i` originally).

  If `slice_value = TRUE`, `value` must be size 1 or the same size as
  `x`.

- slice_value:

  A boolean. If `TRUE`, the assignment proceeds as if you had provided
  `vec_slice(x, i) <- vec_slice(value, i)`, but is optimized to avoid
  materializing the slice of `value`.

- x_arg, value_arg:

  Argument names for `x` and `value`. These are used in error messages
  to inform the user about the locations of incompatible types and sizes
  (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)
  and
  [`stop_incompatible_size()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)).

## Value

A vector of the same type as `x`.

## Genericity

Support for S3 objects depends on whether the object implements a
[`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md) method.

- When a [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
  method exists, the proxy is sliced or assigned to and
  [`vec_restore()`](https://vctrs.r-lib.org/reference/vec_proxy.md) is
  called on the result.

- Otherwise, `vec_slice()` falls back to the base generic `[` and
  `vec_slice<-()` falls back to the base generic `[<-`.

When `vec_slice<-()` falls back to `[<-`, it is expected that the
subclass's `[<-` method can handle the following subset of cases that
base R's `[<-` can also handle:

- An `i` vector of positive integer positions (notably excluding `NA`).

- A `value` vector of length 1 or length `length(i)`. If length 1, it
  should be recycled by the `[<-` method to the length of `i`.

If your `[<-` method eventually calls base R's native `[<-` code, then
these cases will be handled for you.

Note that S3 lists are treated as scalars by default, and will cause an
error if they don't implement a
[`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md) method.

## Differences with base R subsetting

- `vec_slice()` only slices along one dimension. For two-dimensional
  types, the first dimension is subsetted.

- `vec_slice()` preserves attributes by default.

- `vec_slice<-()` is type-stable and always returns the same type as the
  LHS.

## Dependencies

### vctrs dependencies

- [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)

- [`vec_restore()`](https://vctrs.r-lib.org/reference/vec_proxy.md)

### base dependencies

- [`` base::`[` ``](https://rdrr.io/r/base/Extract.html)

- `` base::`[<-` ``

## Examples

``` r
x <- sample(10)
x
#>  [1]  6  3  1  4 10  8  7  2  5  9
vec_slice(x, 1:3)
#> [1] 6 3 1

# You can assign with the infix variant:
vec_slice(x, 2) <- 100
x
#>  [1]   6 100   1   4  10   8   7   2   5   9

# Or with the regular variant that doesn't modify the original input:
y <- vec_assign(x, 3, 500)
y
#>  [1]   6 100 500   4  10   8   7   2   5   9
x
#>  [1]   6 100   1   4  10   8   7   2   5   9


# Slicing objects of higher dimension:
vec_slice(mtcars, 1:3)
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1

# Type stability --------------------------------------------------

# The assign variant is type stable. It always returns the same
# type as the input.
x <- 1:5
vec_slice(x, 2) <- 20.0

# `x` is still an integer vector because the RHS was cast to the
# type of the LHS:
vec_ptype(x)
#> integer(0)

# Compare to `[<-`:
x[2] <- 20.0
vec_ptype(x)
#> numeric(0)


# Note that the types must be coercible for the cast to happen.
# For instance, you can cast a double vector of whole numbers to an
# integer vector:
vec_cast(1, integer())
#> [1] 1

# But not fractional doubles:
try(vec_cast(1.5, integer()))
#> Error in eval(expr, envir) : 
#>   Can't convert from `1.5` <double> to <integer> due to loss of precision.
#> • Locations: 1

# For this reason you can't assign fractional values in an integer
# vector:
x <- 1:3
try(vec_slice(x, 2) <- 1.5)
#> Error in `vec_slice<-`(`*tmp*`, 2, value = 1.5) : 
#>   Can't convert from `1.5` <double> to <integer> due to loss of precision.
#> • Locations: 1

# Slicing `value` -------------------------------------------------

# Sometimes both `x` and `value` start from objects that are the same length,
# and you need to slice `value` by `i` before assigning it to `x`. This comes
# up when thinking about how `base::ifelse()` and `dplyr::case_when()` work.
condition <- c(TRUE, FALSE, TRUE, FALSE)
yes <- 1:4
no <- 5:8

# Create an output container and fill it
out <- vec_init(integer(), 4)
out <- vec_assign(out, condition, vec_slice(yes, condition))
out <- vec_assign(out, !condition, vec_slice(no, !condition))
out
#> [1] 1 6 3 8

# This is wasteful because you have to materialize the slices of `yes` and
# `no` before they can be assigned, and you also have to validate `condition`
# multiple times. Using `slice_value` internally performs
# `vec_slice(yes, condition)` and `vec_slice(no, !condition)` for you,
# but does so in a way that avoids the materialization.
out <- vec_init(integer(), 4)
out <- vec_assign(out, condition, yes, slice_value = TRUE)
out <- vec_assign(out, !condition, no, slice_value = TRUE)
out
#> [1] 1 6 3 8
```
