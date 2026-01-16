# Split a vector into groups

This is a generalisation of
[`split()`](https://rdrr.io/r/base/split.html) that can split by any
type of vector, not just factors. Instead of returning the keys in the
character names, the are returned in a separate parallel vector.

## Usage

``` r
vec_split(x, by)
```

## Arguments

- x:

  Vector to divide into groups.

- by:

  Vector whose unique values defines the groups.

## Value

A data frame with two columns and size equal to
`vec_size(vec_unique(by))`. The `key` column has the same type as `by`,
and the `val` column is a list containing elements of type
`vec_ptype(x)`.

Note for complex types, the default `data.frame` print method will be
suboptimal, and you will want to coerce into a tibble to better
understand the output.

## Dependencies

- [`vec_group_loc()`](https://vctrs.r-lib.org/reference/vec_group.md)

- [`vec_chop()`](https://vctrs.r-lib.org/reference/vec_chop.md)

## Examples

``` r
vec_split(mtcars$cyl, mtcars$vs)
#>   key                                                  val
#> 1   0 6, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 8, 6, 8
#> 2   1             4, 6, 6, 4, 4, 6, 6, 4, 4, 4, 4, 4, 4, 4
vec_split(mtcars$cyl, mtcars[c("vs", "am")])
#>   key.vs key.am                                val
#> 1      0      1                   6, 6, 4, 8, 6, 8
#> 2      1      1                4, 4, 4, 4, 4, 4, 4
#> 3      1      0                6, 6, 4, 4, 6, 6, 4
#> 4      0      0 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8

if (require("tibble")) {
  as_tibble(vec_split(mtcars$cyl, mtcars[c("vs", "am")]))
  as_tibble(vec_split(mtcars, mtcars[c("vs", "am")]))
}
#> # A tibble: 4 × 2
#>   key$vs   $am val           
#>    <dbl> <dbl> <list>        
#> 1      0     1 <df [6 × 11]> 
#> 2      1     1 <df [7 × 11]> 
#> 3      1     0 <df [7 × 11]> 
#> 4      0     0 <df [12 × 11]>
```
