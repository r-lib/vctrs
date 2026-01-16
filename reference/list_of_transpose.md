# Transpose a list of homogenous vectors

`list_of_transpose()` takes a list of homogenous vectors, transposes it,
and returns a new list of homogenous vectors. To perform a transpose,
three pieces of information are required:

- The *list size*, from
  [`vec_size(x)`](https://vctrs.r-lib.org/reference/vec_size.md).

- The *element size*, from
  [`list_of_size(x)`](https://vctrs.r-lib.org/reference/list-of-attributes.md).

- The *element type*, from
  [`list_of_ptype(x)`](https://vctrs.r-lib.org/reference/list-of-attributes.md).

Because all three of these are required, this function only works on
fully specified
[`list_of()`](https://vctrs.r-lib.org/reference/list_of.md)s, with both
`size` and `ptype` specified.

To predict the output from `list_of_transpose()`, swap the list size
with the element size. For example:

- Input: `list_of<integer[3]>[2]`

- Output: `list_of<integer[2]>[3]`

## Usage

``` r
list_of_transpose(x, ..., x_arg = caller_arg(x), error_call = current_env())
```

## Arguments

- x:

  A [list_of](https://vctrs.r-lib.org/reference/list_of.md) with both
  `size` and `ptype` specified.

- ...:

  These dots are for future extensions and must be empty.

- x_arg:

  Argument name used in error messages.

- error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A `list_of` of size `list_of_size(x)`, with an element size of
`vec_size(x)` and an element type of `list_of_ptype(x)`.

## Examples

``` r
# A form of `list_of()` that infers both ptype and size
list_of2 <- function(...) {
  list_of(..., .ptype = NULL, .size = NULL)
}

# I: list_of<integer[2]>[3]
# O: list_of<integer[3]>[2]
list_of_transpose(list_of2(1:2, 3:4, 5:6))
#> <list_of<integer[3]>[2]>
#> [[1]]
#> [1] 1 3 5
#> 
#> [[2]]
#> [1] 2 4 6
#> 

# With data frames
x <- data_frame(a = 1:2, b = letters[1:2])
y <- data_frame(a = 3:4, b = letters[3:4])
list_of_transpose(list_of2(x, y))
#> <list_of<
#>   data.frame<
#>     a: integer
#>     b: character
#>   >[2]
#> >[2]>
#> [[1]]
#>   a b
#> 1 1 a
#> 2 3 c
#> 
#> [[2]]
#>   a b
#> 1 2 b
#> 2 4 d
#> 

# Size 1 elements are recycled
list_of_transpose(list_of2(1, 2:3, 4))
#> <list_of<double[3]>[2]>
#> [[1]]
#> [1] 1 2 4
#> 
#> [[2]]
#> [1] 1 3 4
#> 

# ---------------------------------------------------------------------------
# `NULL` handling

# `NULL` values aren't allowed in `list_of_transpose()`
x <- list_of2(1:3, NULL, 5:7, NULL)
try(list_of_transpose(x))
#> Error in list_of_transpose(x) : `x` can't contain `NULL` values.

# Either drop them entirely or replace them up front before transposing

x_dropped <- vec_slice(x, !vec_detect_missing(x))
x_dropped
#> <list_of<integer[3]>[2]>
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 5 6 7
#> 

list_of_transpose(x_dropped)
#> <list_of<integer[2]>[3]>
#> [[1]]
#> [1] 1 5
#> 
#> [[2]]
#> [1] 2 6
#> 
#> [[3]]
#> [1] 3 7
#> 

x_replaced <- vec_assign(x, vec_detect_missing(x), list(NA))
x_replaced
#> <list_of<integer[3]>[4]>
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] NA NA NA
#> 
#> [[3]]
#> [1] 5 6 7
#> 
#> [[4]]
#> [1] NA NA NA
#> 

list_of_transpose(x_replaced)
#> <list_of<integer[4]>[3]>
#> [[1]]
#> [1]  1 NA  5 NA
#> 
#> [[2]]
#> [1]  2 NA  6 NA
#> 
#> [[3]]
#> [1]  3 NA  7 NA
#> 

# ---------------------------------------------------------------------------
# Reversibility

# Because `list_of_transpose()` takes and returns fully specified list-ofs,
# it is fully reversible, even in the edge cases.
x <- list_of2(integer(), integer())

# This returns a list of size 0
# I: list_of<integer[0]>[2]
# O: list_of<integer[2]>[0]
out <- list_of_transpose(x)
out
#> <list_of<integer[2]>[0]>

# Even though there are no elements, we know the element size and type,
# so we can transpose a second time to recover `x`. This would not be
# possible if this function returned a bare `list()`, which would result
# in lost information.
# I: list_of<integer[2]>[0]
# O: list_of<integer[0]>[2]
list_of_transpose(out)
#> <list_of<integer[0]>[2]>
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> integer(0)
#> 

# ---------------------------------------------------------------------------
# Padding

# If you'd like to pad with a missing value rather than erroring,
# you might do something like this, which left-pads before conversion
# to list-of.
x <- list(1, 2:5, 6:7)

sizes <- list_sizes(x)
size <- max(sizes)
index <- which(sizes != size)

x[index] <- lapply(
  index,
  function(i) vec_c(rep(NA, times = size - sizes[[i]]), x[[i]])
)
x
#> [[1]]
#> [1] NA NA NA  1
#> 
#> [[2]]
#> [1] 2 3 4 5
#> 
#> [[3]]
#> [1] NA NA  6  7
#> 

x <- as_list_of(x, .ptype = NULL, .size = NULL)

list_of_transpose(x)
#> <list_of<double[3]>[4]>
#> [[1]]
#> [1] NA  2 NA
#> 
#> [[2]]
#> [1] NA  3 NA
#> 
#> [[3]]
#> [1] NA  4  6
#> 
#> [[4]]
#> [1] 1 5 7
#> 
```
