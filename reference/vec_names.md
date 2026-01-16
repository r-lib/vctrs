# Get or set the names of a vector

These functions work like
[`rlang::names2()`](https://rlang.r-lib.org/reference/names2.html),
[`names()`](https://rdrr.io/r/base/names.html) and
[`names<-()`](https://rdrr.io/r/base/names.html), except that they
return or modify the the rowwise names of the vector. These are:

- The usual [`names()`](https://rdrr.io/r/base/names.html) for atomic
  vectors and lists

- The row names for data frames and matrices

- The names of the first dimension for arrays Rowwise names are size
  consistent: the length of the names always equals
  [`vec_size()`](https://vctrs.r-lib.org/reference/vec_size.md).

`vec_names2()` returns the repaired names from a vector, even if it is
unnamed. See
[`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.md)
for details on name repair.

`vec_names()` is a bare-bones version that returns `NULL` if the vector
is unnamed.

`vec_set_names()` sets the names or removes them.

## Usage

``` r
vec_names2(
  x,
  ...,
  repair = c("minimal", "unique", "universal", "check_unique", "unique_quiet",
    "universal_quiet"),
  quiet = FALSE
)

vec_names(x)

vec_set_names(x, names)
```

## Arguments

- x:

  A vector with names

- ...:

  These dots are for future extensions and must be empty.

- repair:

  Either a string or a function. If a string, it must be one of
  `"check_unique"`, `"minimal"`, `"unique"`, `"universal"`,
  `"unique_quiet"`, or `"universal_quiet"`. If a function, it is invoked
  with a vector of minimal names and must return minimal names,
  otherwise an error is thrown.

  - Minimal names are never `NULL` or `NA`. When an element doesn't have
    a name, its minimal name is an empty string.

  - Unique names are unique. A suffix is appended to duplicate names to
    make them unique.

  - Universal names are unique and syntactic, meaning that you can
    safely use the names as variables without causing a syntax error.

  The `"check_unique"` option doesn't perform any name repair. Instead,
  an error is raised if the names don't suit the `"unique"` criteria.

  The options `"unique_quiet"` and `"universal_quiet"` are here to help
  the user who calls this function indirectly, via another function
  which exposes `repair` but not `quiet`. Specifying
  `repair = "unique_quiet"` is like specifying
  `repair = "unique", quiet = TRUE`. When the `"*_quiet"` options are
  used, any setting of `quiet` is silently overridden.

- quiet:

  By default, the user is informed of any renaming caused by repairing
  the names. This only concerns unique and universal repairing. Set
  `quiet` to `TRUE` to silence the messages.

  Users can silence the name repair messages by setting the
  `"rlib_name_repair_verbosity"` global option to `"quiet"`.

- names:

  A character vector, or `NULL`.

## Value

`vec_names2()` returns the names of `x`, repaired. `vec_names()` returns
the names of `x` or `NULL` if unnamed. `vec_set_names()` returns `x`
with names updated.

## Examples

``` r
vec_names2(1:3)
#> [1] "" "" ""
vec_names2(1:3, repair = "unique")
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> [1] "...1" "...2" "...3"
vec_names2(c(a = 1, b = 2))
#> [1] "a" "b"

# `vec_names()` consistently returns the rowwise names of data frames and arrays:
vec_names(data.frame(a = 1, b = 2))
#> NULL
names(data.frame(a = 1, b = 2))
#> [1] "a" "b"
vec_names(mtcars)
#>  [1] "Mazda RX4"           "Mazda RX4 Wag"       "Datsun 710"         
#>  [4] "Hornet 4 Drive"      "Hornet Sportabout"   "Valiant"            
#>  [7] "Duster 360"          "Merc 240D"           "Merc 230"           
#> [10] "Merc 280"            "Merc 280C"           "Merc 450SE"         
#> [13] "Merc 450SL"          "Merc 450SLC"         "Cadillac Fleetwood" 
#> [16] "Lincoln Continental" "Chrysler Imperial"   "Fiat 128"           
#> [19] "Honda Civic"         "Toyota Corolla"      "Toyota Corona"      
#> [22] "Dodge Challenger"    "AMC Javelin"         "Camaro Z28"         
#> [25] "Pontiac Firebird"    "Fiat X1-9"           "Porsche 914-2"      
#> [28] "Lotus Europa"        "Ford Pantera L"      "Ferrari Dino"       
#> [31] "Maserati Bora"       "Volvo 142E"         
names(mtcars)
#>  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"  
#> [10] "gear" "carb"
vec_names(Titanic)
#> [1] "1st"  "2nd"  "3rd"  "Crew"
names(Titanic)
#> NULL

vec_set_names(1:3, letters[1:3])
#> a b c 
#> 1 2 3 
vec_set_names(data.frame(a = 1:3), letters[1:3])
#>   a
#> a 1
#> b 2
#> c 3
```
