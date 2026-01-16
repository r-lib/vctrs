# Construct a list of homogenous vectors

A `list_of` is a restricted version of a list that adds constraints on
the list elements.

- `list_of(.ptype = )` restricts the *type* of each element.

  - `.ptype = <type>` asserts that each element has type `<type>`.

  - `.ptype = NULL` infers the type from the original set of elements,
    or errors if no vector inputs were provided.

  - `.ptype = rlang::zap()` doesn't restrict the type.

- `list_of(.size = )` restricts the *size* of each element.

  - `.size = <size>` asserts that each element has size `<size>`.

  - `.size = NULL` infers the size from the original set of elements, or
    errors if no vector inputs were provided.

  - `.size = rlang::zap()` doesn't restrict the size.

The default behavior infers the element type and doesn't restrict the
size.

Both `.ptype` and `.size` may be specified to restrict both the size and
type of the list elements. You cannot set both of these to
[`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html), as that
would be the same as a bare [`list()`](https://rdrr.io/r/base/list.html)
with no restrictions.

Modifying a `list_of` with `$<-`, `[<-`, and `[[<-` preserves the
constraints by coercing and recycling all input items.

## Usage

``` r
list_of(..., .ptype = NULL, .size = zap())

as_list_of(x, ...)

is_list_of(x)

# S3 method for class 'vctrs_list_of'
vec_ptype2(x, y, ..., x_arg = "", y_arg = "")

# S3 method for class 'vctrs_list_of'
vec_cast(x, to, ...)
```

## Arguments

- ...:

  For `list_of()`, vectors to include in the list.

  For other methods, these dots must be empty.

- .ptype:

  The type to restrict each list element to. One of:

  - A prototype like [`integer()`](https://rdrr.io/r/base/integer.html)
    or [`double()`](https://rdrr.io/r/base/double.html).

  - `NULL`, to infer the type from `...`. If no vector inputs are
    provided, an error is thrown.

  - [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html) to
    avoid placing any restrictions on the type.

- .size:

  The size to restrict each list element to. One of:

  - A scalar integer size.

  - `NULL`, to infer the size from `...`. If no vector inputs are
    provided, an error is thrown.

  - [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html) to
    avoid placing any restrictions on the size.

- x:

  For `as_list_of()`, a vector to be coerced to list_of.

  For `is_list_of()`, an object to test.

- y, to:

  Arguments to
  [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) and
  [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md).

- x_arg, y_arg:

  Argument names for `x` and `y`. These are used in error messages to
  inform the user about the locations of incompatible types (see
  [`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)).

## Examples

``` r
# Restrict the type, but not the size
x <- list_of(1:3, 5:6, 10:15)
x
#> <list_of<integer>[3]>
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 5 6
#> 
#> [[3]]
#> [1] 10 11 12 13 14 15
#> 

if (requireNamespace("tibble", quietly = TRUE)) {
  # As a column in a tibble
  tibble::tibble(x = x)
}
#> # A tibble: 3 × 1
#>             x
#>   <list<int>>
#> 1         [3]
#> 2         [2]
#> 3         [6]

# Coercion happens during assignment
x[1] <- list(4)
typeof(x[[1]])
#> [1] "integer"

try(x[1] <- list(4.5))
#> Error in lapply(.x, .f, ...) : 
#>   Can't convert from `X[[i]]` <double> to <integer> due to loss of precision.
#> • Locations: 1

# Restrict the size, but not the type
x <- list_of(1, 2:3, .ptype = rlang::zap(), .size = 2)
x
#> <list_of<any[2]>[2]>
#> [[1]]
#> [1] 1 1
#> 
#> [[2]]
#> [1] 2 3
#> 

# Recycling happens during assignment
x[1] <- list(4)
x
#> <list_of<any[2]>[2]>
#> [[1]]
#> [1] 4 4
#> 
#> [[2]]
#> [1] 2 3
#> 

try(x[1] <- list(3:6))
#> Error in lapply(.x, .f, ...) : Can't recycle input of size 4 to size 2.

# Restricting both size and type
x <- list_of(1L, 2:3, .ptype = integer(), .size = 2)
x
#> <list_of<integer[2]>[2]>
#> [[1]]
#> [1] 1 1
#> 
#> [[2]]
#> [1] 2 3
#> 

# Setting an element to `NULL`
x[2] <- list(NULL)
x
#> <list_of<integer[2]>[2]>
#> [[1]]
#> [1] 1 1
#> 
#> [[2]]
#> NULL
#> 

# Note that using `NULL` shortens the list, like a base R list
x[2] <- NULL
x
#> <list_of<integer[2]>[1]>
#> [[1]]
#> [1] 1 1
#> 

# Combining a list_of with a list results in a list
vec_c(list_of(1), list(2, "x"))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] "x"
#> 

# Combining a list_of with another list_of tries to find a common element
# type and common element size, but will remove the constraint if that
# fails
x <- list_of(1, .ptype = double())
y <- list_of(c("a", "b"), .ptype = character(), .size = 2)
z <- list_of(c("c", "d", "e"), .ptype = character(), .size = 3)

# Falls back to a list
vec_c(x, y)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] "a" "b"
#> 

# Falls back to a `list_of<character>` with no size restriction
vec_c(y, z)
#> <list_of<character>[2]>
#> [[1]]
#> [1] "a" "b"
#> 
#> [[2]]
#> [1] "c" "d" "e"
#> 
```
