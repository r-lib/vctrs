# FAQ - How does recycling work in vctrs and the tidyverse?

Recycling describes the concept of repeating elements of one vector to
match the size of another. There are two rules that underlie the
“tidyverse” recycling rules:

- Vectors of size 1 will be recycled to the size of any other vector

- Otherwise, all vectors must have the same size

## Examples

Vectors of size 1 are recycled to the size of any other vector:

    tibble(x = 1:3, y = 1L)
    #> # A tibble: 3 x 2
    #>       x     y
    #>   <int> <int>
    #> 1     1     1
    #> 2     2     1
    #> 3     3     1

This includes vectors of size 0:

    tibble(x = integer(), y = 1L)
    #> # A tibble: 0 x 2
    #> # i 2 variables: x <int>, y <int>

If vectors aren’t size 1, they must all be the same size. Otherwise, an
error is thrown:

    tibble(x = 1:3, y = 4:7)
    #> Error in `tibble()`:
    #> ! Tibble columns must have compatible sizes.
    #> * Size 3: Existing data.
    #> * Size 4: Column `y`.
    #> i Only values of size one are recycled.

## vctrs backend

Packages in r-lib and the tidyverse generally use
[`vec_size_common()`](https://vctrs.r-lib.org/dev/reference/vec_size.md)
and
[`vec_recycle_common()`](https://vctrs.r-lib.org/dev/reference/vec_recycle.md)
as the backends for handling recycling rules.

- [`vec_size_common()`](https://vctrs.r-lib.org/dev/reference/vec_size.md)
  returns the common size of multiple vectors, after applying the
  recycling rules

- [`vec_recycle_common()`](https://vctrs.r-lib.org/dev/reference/vec_recycle.md)
  goes one step further, and actually recycles the vectors to their
  common size

    vec_size_common(1:3, "x")
    #> [1] 3

    vec_recycle_common(1:3, "x")
    #> [[1]]
    #> [1] 1 2 3
    #>
    #> [[2]]
    #> [1] "x" "x" "x"

    vec_size_common(1:3, c("x", "y"))
    #> Error:
    #> ! Can't recycle `..1` (size 3) to match `..2` (size 2).

## Base R recycling rules

The recycling rules described here are stricter than the ones generally
used by base R, which are:

- If any vector is length 0, the output will be length 0

- Otherwise, the output will be length `max(length_x, length_y)`, and a
  warning will be thrown if the length of the longer vector is not an
  integer multiple of the length of the shorter vector.

We explore the base R rules in detail in
[`vignette("type-size")`](https://vctrs.r-lib.org/dev/articles/type-size.md).
