# Tools for accessing the fields of a record.

A [rcrd](https://vctrs.r-lib.org/reference/new_rcrd.md) behaves like a
vector, so [`length()`](https://rdrr.io/r/base/length.html),
[`names()`](https://rdrr.io/r/base/names.html), and `$` can not provide
access to the fields of the underlying list. These helpers do:
`fields()` is equivalent to
[`names()`](https://rdrr.io/r/base/names.html); `n_fields()` is
equivalent to [`length()`](https://rdrr.io/r/base/length.html);
`field()` is equivalent to `$`.

## Usage

``` r
fields(x)

n_fields(x)

field(x, i)

field(x, i) <- value
```

## Arguments

- x:

  A [rcrd](https://vctrs.r-lib.org/reference/new_rcrd.md), i.e. a list
  of equal length vectors with unique names.

## Examples

``` r
x <- new_rcrd(list(x = 1:3, y = 3:1, z = letters[1:3]))
n_fields(x)
#> [1] 3
fields(x)
#> [1] "x" "y" "z"

field(x, "y")
#> [1] 3 2 1
field(x, "y") <- runif(3)
field(x, "y")
#> [1] 0.4977774 0.2897672 0.7328820
```
