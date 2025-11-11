# Partially specify a factor

**\[experimental\]**

This special class can be passed as a `ptype` in order to specify that
the result should be a factor that contains at least the specified
levels.

## Usage

``` r
partial_factor(levels = character())
```

## Arguments

- levels:

  Character vector of labels.

## Examples

``` r
pf <- partial_factor(levels = c("x", "y"))
pf
#> partial_factor<
#>   5ec15 {partial}
#> >

vec_ptype_common(factor("v"), factor("w"), .ptype = pf)
#> factor()
#> Levels: v w x y
```
