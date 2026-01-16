# Vector recycling

`vec_recycle(x, size)` recycles a single vector to a given size.
`vec_recycle_common(...)` recycles multiple vectors to their common
size. All functions obey the [vctrs recycling
rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.md), and
will throw an error if recycling is not possible. See
[`vec_size()`](https://vctrs.r-lib.org/reference/vec_size.md) for the
precise definition of size.

## Usage

``` r
vec_recycle(x, size, ..., x_arg = "", call = caller_env())

vec_recycle_common(..., .size = NULL, .arg = "", .call = caller_env())
```

## Arguments

- x:

  A vector to recycle.

- size:

  Desired output size.

- ...:

  Depending on the function used:

  - For `vec_recycle_common()`, vectors to recycle.

  - For `vec_recycle()`, these dots should be empty.

- x_arg:

  Argument name for `x`. These are used in error messages to inform the
  user about which argument has an incompatible size.

- call, .call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- .size:

  Desired output size. If omitted, will use the common size from
  [`vec_size_common()`](https://vctrs.r-lib.org/reference/vec_size.md).

- .arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

## Dependencies

- [`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md)

## Examples

``` r
# Inputs with 1 observation are recycled
vec_recycle_common(1:5, 5)
#> [[1]]
#> [1] 1 2 3 4 5
#> 
#> [[2]]
#> [1] 5 5 5 5 5
#> 
vec_recycle_common(integer(), 5)
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> numeric(0)
#> 
if (FALSE) { # \dontrun{
vec_recycle_common(1:5, 1:2)
} # }

# Data frames and matrices are recycled along their rows
vec_recycle_common(data.frame(x = 1), 1:5)
#> [[1]]
#>   x
#> 1 1
#> 2 1
#> 3 1
#> 4 1
#> 5 1
#> 
#> [[2]]
#> [1] 1 2 3 4 5
#> 
vec_recycle_common(array(1:2, c(1, 2)), 1:5)
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    2
#> [3,]    1    2
#> [4,]    1    2
#> [5,]    1    2
#> 
#> [[2]]
#> [1] 1 2 3 4 5
#> 
vec_recycle_common(array(1:3, c(1, 3, 1)), 1:5)
#> [[1]]
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    1    2    3
#> [3,]    1    2    3
#> [4,]    1    2    3
#> [5,]    1    2    3
#> 
#> 
#> [[2]]
#> [1] 1 2 3 4 5
#> 
```
