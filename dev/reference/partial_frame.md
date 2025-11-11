# Partially specify columns of a data frame

**\[experimental\]**

This special class can be passed to `.ptype` in order to specify the
types of only some of the columns in a data frame.

## Usage

``` r
partial_frame(...)
```

## Arguments

- ...:

  Attributes of subclass

## Examples

``` r
pf <- partial_frame(x = double())
pf
#> partial_frame<
#>   x: double {partial}
#> >

vec_rbind(
  data.frame(x = 1L, y = "a"),
  data.frame(x = FALSE, z = 10),
  .ptype = partial_frame(x = double(), a = character())
)
#>   x    y  z    a
#> 1 1    a NA <NA>
#> 2 0 <NA> 10 <NA>
```
