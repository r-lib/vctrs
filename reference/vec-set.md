# Set operations

- `vec_set_intersect()` returns all values in both `x` and `y`.

- `vec_set_difference()` returns all values in `x` but not `y`. Note
  that this is an asymmetric set difference, meaning it is not
  commutative.

- `vec_set_union()` returns all values in either `x` or `y`.

- `vec_set_symmetric_difference()` returns all values in either `x` or
  `y` but not both. This is a commutative difference.

Because these are *set* operations, these functions only return unique
values from `x` and `y`, returned in the order they first appeared in
the original input. Names of `x` and `y` are retained on the result, but
names are always taken from `x` if the value appears in both inputs.

These functions work similarly to
[`base::intersect()`](https://rdrr.io/r/base/sets.html),
[`base::setdiff()`](https://rdrr.io/r/base/sets.html), and
[`base::union()`](https://rdrr.io/r/base/sets.html), but don't strip
attributes and can be used with data frames.

## Usage

``` r
vec_set_intersect(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
)

vec_set_difference(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
)

vec_set_union(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
)

vec_set_symmetric_difference(
  x,
  y,
  ...,
  ptype = NULL,
  x_arg = "x",
  y_arg = "y",
  error_call = current_env()
)
```

## Arguments

- x, y:

  A pair of vectors.

- ...:

  These dots are for future extensions and must be empty.

- ptype:

  If `NULL`, the default, the output type is determined by computing the
  common type between `x` and `y`. If supplied, both `x` and `y` will be
  cast to this type.

- x_arg, y_arg:

  Argument names for `x` and `y`. These are used in error messages.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A vector of the common type of `x` and `y` (or `ptype`, if supplied)
containing the result of the corresponding set function.

## Details

Missing values are treated as equal to other missing values. For doubles
and complexes, `NaN` are equal to other `NaN`, but not to `NA`.

## Dependencies

### `vec_set_intersect()`

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.md)

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

- [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)

- [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)

### `vec_set_difference()`

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.md)

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

- [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)

- [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)

### `vec_set_union()`

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.md)

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

- [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)

- [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)

- [`vec_c()`](https://vctrs.r-lib.org/reference/vec_c.md)

### `vec_set_symmetric_difference()`

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/reference/vec_proxy_equal.md)

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

- [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)

- [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)

- [`vec_c()`](https://vctrs.r-lib.org/reference/vec_c.md)

## Examples

``` r
x <- c(1, 2, 1, 4, 3)
y <- c(2, 5, 5, 1)

# All unique values in both `x` and `y`.
# Duplicates in `x` and `y` are always removed.
vec_set_intersect(x, y)
#> [1] 1 2

# All unique values in `x` but not `y`
vec_set_difference(x, y)
#> [1] 4 3

# All unique values in either `x` or `y`
vec_set_union(x, y)
#> [1] 1 2 4 3 5

# All unique values in either `x` or `y` but not both
vec_set_symmetric_difference(x, y)
#> [1] 4 3 5

# These functions can also be used with data frames
x <- data_frame(
  a = c(2, 3, 2, 2),
  b = c("j", "k", "j", "l")
)
#> Warning: `data_frame()` was deprecated in tibble 1.1.0.
#> ℹ Please use `tibble()` instead.
y <- data_frame(
  a = c(1, 2, 2, 2, 3),
  b = c("j", "l", "j", "l", "j")
)

vec_set_intersect(x, y)
#> # A tibble: 2 × 2
#>       a b    
#>   <dbl> <chr>
#> 1     2 j    
#> 2     2 l    
vec_set_difference(x, y)
#> # A tibble: 1 × 2
#>       a b    
#>   <dbl> <chr>
#> 1     3 k    
vec_set_union(x, y)
#> # A tibble: 5 × 2
#>       a b    
#>   <dbl> <chr>
#> 1     2 j    
#> 2     3 k    
#> 3     2 l    
#> 4     1 j    
#> 5     3 j    
vec_set_symmetric_difference(x, y)
#> # A tibble: 3 × 2
#>       a b    
#>   <dbl> <chr>
#> 1     3 k    
#> 2     1 j    
#> 3     3 j    

# Vector names don't affect set membership, but if you'd like to force
# them to, you can transform the vector into a two column data frame
x <- c(a = 1, b = 2, c = 2, d = 3)
y <- c(c = 2, b = 1, a = 3, d = 3)

vec_set_intersect(x, y)
#> a b d 
#> 1 2 3 

x <- data_frame(name = names(x), value = unname(x))
y <- data_frame(name = names(y), value = unname(y))

vec_set_intersect(x, y)
#> # A tibble: 2 × 2
#>   name  value
#>   <chr> <dbl>
#> 1 c         2
#> 2 d         3
```
