# Repeat a vector

- `vec_rep()` repeats an entire vector a set number of `times`.

- `vec_rep_each()` repeats each element of a vector a set number of
  `times`.

- `vec_unrep()` compresses a vector with repeated values. The repeated
  values are returned as a `key` alongside the number of `times` each
  key is repeated.

## Usage

``` r
vec_rep(
  x,
  times,
  ...,
  error_call = current_env(),
  x_arg = "x",
  times_arg = "times"
)

vec_rep_each(
  x,
  times,
  ...,
  error_call = current_env(),
  x_arg = "x",
  times_arg = "times"
)

vec_unrep(x)
```

## Arguments

- x:

  A vector.

- times:

  For `vec_rep()`, a single integer for the number of times to repeat
  the entire vector.

  For `vec_rep_each()`, an integer vector of the number of times to
  repeat each element of `x`. `times` will be
  [recycled](https://vctrs.r-lib.org/dev/reference/theory-faq-recycling.md)
  to the size of `x`.

- ...:

  These dots are for future extensions and must be empty.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- x_arg, times_arg:

  Argument names for errors.

## Value

For `vec_rep()`, a vector the same type as `x` with size
`vec_size(x) * times`.

For `vec_rep_each()`, a vector the same type as `x` with size
`sum(vec_recycle(times, vec_size(x)))`.

For `vec_unrep()`, a data frame with two columns, `key` and `times`.
`key` is a vector with the same type as `x`, and `times` is an integer
vector.

## Details

Using `vec_unrep()` and `vec_rep_each()` together is similar to using
[`base::rle()`](https://rdrr.io/r/base/rle.html) and
[`base::inverse.rle()`](https://rdrr.io/r/base/rle.html). The following
invariant shows the relationship between the two functions:

    compressed <- vec_unrep(x)
    identical(x, vec_rep_each(compressed$key, compressed$times))

There are two main differences between `vec_unrep()` and
[`base::rle()`](https://rdrr.io/r/base/rle.html):

- `vec_unrep()` treats adjacent missing values as equivalent, while
  [`rle()`](https://rdrr.io/r/base/rle.html) treats them as different
  values.

- `vec_unrep()` works along the size of `x`, while
  [`rle()`](https://rdrr.io/r/base/rle.html) works along its length.
  This means that `vec_unrep()` works on data frames by compressing
  repeated rows.

## Dependencies

- [`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)

## Examples

``` r
# Repeat the entire vector
vec_rep(1:2, 3)
#> [1] 1 2 1 2 1 2

# Repeat within each vector
vec_rep_each(1:2, 3)
#> [1] 1 1 1 2 2 2
x <- vec_rep_each(1:2, c(3, 4))
x
#> [1] 1 1 1 2 2 2 2

# After using `vec_rep_each()`, you can recover the original vector
# with `vec_unrep()`
vec_unrep(x)
#>   key times
#> 1   1     3
#> 2   2     4

df <- data.frame(x = 1:2, y = 3:4)

# `rep()` repeats columns of data frames, and returns lists
rep(df, each = 2)
#> $x
#> [1] 1 2
#> 
#> $x
#> [1] 1 2
#> 
#> $y
#> [1] 3 4
#> 
#> $y
#> [1] 3 4
#> 

# `vec_rep()` and `vec_rep_each()` repeat rows, and return data frames
vec_rep(df, 2)
#>   x y
#> 1 1 3
#> 2 2 4
#> 3 1 3
#> 4 2 4
vec_rep_each(df, 2)
#>   x y
#> 1 1 3
#> 2 1 3
#> 3 2 4
#> 4 2 4

# `rle()` treats adjacent missing values as different
y <- c(1, NA, NA, 2)
rle(y)
#> Run Length Encoding
#>   lengths: int [1:4] 1 1 1 1
#>   values : num [1:4] 1 NA NA 2

# `vec_unrep()` treats them as equivalent
vec_unrep(y)
#>   key times
#> 1   1     1
#> 2  NA     2
#> 3   2     1
```
