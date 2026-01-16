# Initialize a vector

Initialize a vector

## Usage

``` r
vec_init(x, n = 1L)
```

## Arguments

- x:

  Template of vector to initialize.

- n:

  Desired size of result.

## Dependencies

- vec_slice()

## Examples

``` r
vec_init(1:10, 3)
#> [1] NA NA NA
vec_init(Sys.Date(), 5)
#> [1] NA NA NA NA NA

# The "missing" value for a data frame is a row that is entirely missing
vec_init(mtcars, 2)
#>      mpg cyl disp hp drat wt qsec vs am gear carb
#> ...1  NA  NA   NA NA   NA NA   NA NA NA   NA   NA
#> ...2  NA  NA   NA NA   NA NA   NA NA NA   NA   NA

# The "missing" value for a list is `NULL`
vec_init(list(), 3)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> NULL
#> 
```
