# Find matching observations across vectors

`vec_in()` returns a logical vector based on whether `needle` is found
in haystack. `vec_match()` returns an integer vector giving location of
`needle` in `haystack`, or `NA` if it's not found.

## Usage

``` r
vec_match(
  needles,
  haystack,
  ...,
  na_equal = TRUE,
  needles_arg = "",
  haystack_arg = ""
)

vec_in(
  needles,
  haystack,
  ...,
  na_equal = TRUE,
  needles_arg = "",
  haystack_arg = ""
)
```

## Arguments

- needles, haystack:

  Vector of `needles` to search for in vector haystack. `haystack`
  should usually be unique; if not `vec_match()` will only return the
  location of the first match.

  `needles` and `haystack` are coerced to the same type prior to
  comparison.

- ...:

  These dots are for future extensions and must be empty.

- na_equal:

  If `TRUE`, missing values in `needles` can be matched to missing
  values in `haystack`. If `FALSE`, they propagate, missing values in
  `needles` are represented as `NA` in the return value.

- needles_arg, haystack_arg:

  Argument tags for `needles` and `haystack` used in error messages.

## Value

A vector the same length as `needles`. `vec_in()` returns a logical
vector; `vec_match()` returns an integer vector.

## Details

`vec_in()` is equivalent to
[base::%in%](https://rdrr.io/r/base/match.html); `vec_match()` is
equivalent to [`base::match()`](https://rdrr.io/r/base/match.html).

## Missing values

In most cases places in R, missing values are not considered to be
equal, i.e. `NA == NA` is not `TRUE`. The exception is in matching
functions like [`base::match()`](https://rdrr.io/r/base/match.html) and
[`merge()`](https://rdrr.io/r/base/merge.html), where an `NA` will match
another `NA`. By `vec_match()` and `vec_in()` will match `NA`s; but you
can control this behaviour with the `na_equal` argument.

## Dependencies

- [`vec_cast_common()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md)
  with fallback

- [`vec_proxy_equal()`](https://vctrs.r-lib.org/dev/reference/vec_proxy_equal.md)

## Examples

``` r
hadley <- strsplit("hadley", "")[[1]]
vec_match(hadley, letters)
#> [1]  8  1  4 12  5 25

vowels <- c("a", "e", "i", "o", "u")
vec_match(hadley, vowels)
#> [1] NA  1 NA NA  2 NA
vec_in(hadley, vowels)
#> [1] FALSE  TRUE FALSE FALSE  TRUE FALSE

# Only the first index of duplicates is returned
vec_match(c("a", "b"), c("a", "b", "a", "b"))
#> [1] 1 2
```
