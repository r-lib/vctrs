# Identify groups

- `vec_group_id()` returns an identifier for the group that each element
  of `x` falls in, constructed in the order that they appear. The number
  of groups is also returned as an attribute, `n`.

- `vec_group_loc()` returns a data frame containing a `key` column with
  the unique groups, and a `loc` column with the locations of each group
  in `x`.

- `vec_group_rle()` locates groups in `x` and returns them run length
  encoded in the order that they appear. The return value is a rcrd
  object with fields for the `group` identifiers and the run `length` of
  the corresponding group. The number of groups is also returned as an
  attribute, `n`.

## Usage

``` r
vec_group_id(x)

vec_group_loc(x)

vec_group_rle(x)
```

## Arguments

- x:

  A vector

## Value

- `vec_group_id()`: An integer vector with the same size as `x`.

- `vec_group_loc()`: A two column data frame with size equal to
  `vec_size(vec_unique(x))`.

  - A `key` column of type `vec_ptype(x)`

  - A `loc` column of type list, with elements of type integer.

- `vec_group_rle()`: A `vctrs_group_rle` rcrd object with two integer
  vector fields: `group` and `length`.

Note that when using `vec_group_loc()` for complex types, the default
`data.frame` print method will be suboptimal, and you will want to
coerce into a tibble to better understand the output.

## Dependencies

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md)

## Examples

``` r
purrr <- c("p", "u", "r", "r", "r")
vec_group_id(purrr)
#> [1] 1 2 3 3 3
#> attr(,"n")
#> [1] 3
vec_group_rle(purrr)
#> <vctrs_group_rle[3][n = 3]>
#> [1] 1x1 2x1 3x3

groups <- mtcars[c("vs", "am")]
vec_group_id(groups)
#>  [1] 1 1 2 3 4 3 4 3 3 3 3 4 4 4 4 4 4 2 2 2 3 4 4 4 4 2 1 2 1 1 1 2
#> attr(,"n")
#> [1] 4

group_rle <- vec_group_rle(groups)
group_rle
#> <vctrs_group_rle[16][n = 4]>
#>  [1] 1x2 2x1 3x1 4x1 3x1 4x1 3x4 4x6 2x3 3x1 4x4 2x1 1x1 2x1 1x3 2x1

# Access fields with `field()`
field(group_rle, "group")
#>  [1] 1 2 3 4 3 4 3 4 2 3 4 2 1 2 1 2
field(group_rle, "length")
#>  [1] 2 1 1 1 1 1 4 6 3 1 4 1 1 1 3 1

# `vec_group_id()` is equivalent to
vec_match(groups, vec_unique(groups))
#>  [1] 1 1 2 3 4 3 4 3 3 3 3 4 4 4 4 4 4 2 2 2 3 4 4 4 4 2 1 2 1 1 1 2

vec_group_loc(mtcars$vs)
#>   key
#> 1   0
#> 2   1
#>                                                                  loc
#> 1 1, 2, 5, 7, 12, 13, 14, 15, 16, 17, 22, 23, 24, 25, 27, 29, 30, 31
#> 2                  3, 4, 6, 8, 9, 10, 11, 18, 19, 20, 21, 26, 28, 32
vec_group_loc(mtcars[c("vs", "am")])
#>   key.vs key.am                                          loc
#> 1      0      1                         1, 2, 27, 29, 30, 31
#> 2      1      1                    3, 18, 19, 20, 26, 28, 32
#> 3      1      0                       4, 6, 8, 9, 10, 11, 21
#> 4      0      0 5, 7, 12, 13, 14, 15, 16, 17, 22, 23, 24, 25

if (require("tibble")) {
  as_tibble(vec_group_loc(mtcars[c("vs", "am")]))
}
#> # A tibble: 4 × 2
#>   key$vs   $am loc       
#>    <dbl> <dbl> <list>    
#> 1      0     1 <int [6]> 
#> 2      1     1 <int [7]> 
#> 3      1     0 <int [7]> 
#> 4      0     0 <int [12]>
```
