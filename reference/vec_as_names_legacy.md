# Repair names with legacy method

This standardises names with the legacy approach that was used in
tidyverse packages (such as tibble, tidyr, and readxl) before
[`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.md)
was implemented. This tool is meant to help transitioning to the new
name repairing standard and will be deprecated and removed from the
package some time in the future.

## Usage

``` r
vec_as_names_legacy(names, prefix = "V", sep = "")
```

## Arguments

- names:

  A character vector.

- prefix, sep:

  Prefix and separator for repaired names.

## Examples

``` r
if (rlang::is_installed("tibble")) {

library(tibble)

# Names repair is turned off by default in tibble:
try(tibble(a = 1, a = 2))

# You can turn it on by supplying a repair method:
tibble(a = 1, a = 2, .name_repair = "universal")

# If you prefer the legacy method, use `vec_as_names_legacy()`:
tibble(a = 1, a = 2, .name_repair = vec_as_names_legacy)

}
#> Error in tibble(a = 1, a = 2) : 
#>   Column name `a` must not be duplicated.
#> Use `.name_repair` to specify repair.
#> Caused by error in `repaired_names()`:
#> ! Names must be unique.
#> ✖ These names are duplicated:
#>   * "a" at locations 1 and 2.
#> New names:
#> • `a` -> `a...1`
#> • `a` -> `a...2`
#> # A tibble: 1 × 2
#>       a    a1
#>   <dbl> <dbl>
#> 1     1     2
```
