# Create a data frame from all combinations of the inputs

`vec_expand_grid()` creates a new data frame by creating a grid of all
possible combinations of the input vectors. It is inspired by
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html). Compared
with [`expand.grid()`](https://rdrr.io/r/base/expand.grid.html), it:

- Produces sorted output by default by varying the first column the
  slowest, rather than the fastest. Control this with `.vary`.

- Never converts strings to factors.

- Does not add additional attributes.

- Drops `NULL` inputs.

- Can expand any vector type, including data frames and
  [records](https://vctrs.r-lib.org/dev/reference/new_rcrd.md).

## Usage

``` r
vec_expand_grid(
  ...,
  .vary = "slowest",
  .name_repair = "check_unique",
  .error_call = current_env()
)
```

## Arguments

- ...:

  Name-value pairs. The name will become the column name in the
  resulting data frame.

- .vary:

  One of:

  - `"slowest"` to vary the first column slowest. This produces sorted
    output and is generally the most useful.

  - `"fastest"` to vary the first column fastest. This matches the
    behavior of
    [`expand.grid()`](https://rdrr.io/r/base/expand.grid.html).

- .name_repair:

  One of `"check_unique"`, `"unique"`, `"universal"`, `"minimal"`,
  `"unique_quiet"`, or `"universal_quiet"`. See
  [`vec_as_names()`](https://vctrs.r-lib.org/dev/reference/vec_as_names.md)
  for the meaning of these options.

- .error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

A data frame with as many columns as there are inputs in `...` and as
many rows as the [`prod()`](https://rdrr.io/r/base/prod.html) of the
sizes of the inputs.

## Details

If any input is empty (i.e. size 0), then the result will have 0 rows.

If no inputs are provided, the result is a 1 row data frame with 0
columns. This is consistent with the fact that
[`prod()`](https://rdrr.io/r/base/prod.html) with no inputs returns `1`.

## Examples

``` r
vec_expand_grid(x = 1:2, y = 1:3)
#>   x y
#> 1 1 1
#> 2 1 2
#> 3 1 3
#> 4 2 1
#> 5 2 2
#> 6 2 3

# Use `.vary` to match `expand.grid()`:
vec_expand_grid(x = 1:2, y = 1:3, .vary = "fastest")
#>   x y
#> 1 1 1
#> 2 2 1
#> 3 1 2
#> 4 2 2
#> 5 1 3
#> 6 2 3

# Can also expand data frames
vec_expand_grid(
  x = data_frame(a = 1:2, b = 3:4),
  y = 1:4
)
#> Warning: corrupt data frame: columns will be truncated or padded with NAs
#>                                                                                         x
#> 1                                                 \033[38;5;246m# A tibble: 8 × 2\033[39m
#> 2                                                                                 a     b
#> 3   \033[3m\033[38;5;246m<int>\033[39m\033[23m \033[3m\033[38;5;246m<int>\033[39m\033[23m
#> 4                                                     \033[38;5;250m1\033[39m     1     3
#> 5                                                     \033[38;5;250m2\033[39m     1     3
#> 6                                                     \033[38;5;250m3\033[39m     1     3
#> 7                                                     \033[38;5;250m4\033[39m     1     3
#> 8                                                     \033[38;5;250m5\033[39m     2     4
#>   y
#> 1 1
#> 2 2
#> 3 3
#> 4 4
#> 5 1
#> 6 2
#> 7 3
#> 8 4
```
