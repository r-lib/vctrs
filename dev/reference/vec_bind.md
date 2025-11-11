# Combine many data frames into one data frame

This pair of functions binds together data frames (and vectors), either
row-wise or column-wise. Row-binding creates a data frame with common
type across all arguments. Column-binding creates a data frame with
common length across all arguments.

## Usage

``` r
vec_rbind(
  ...,
  .ptype = NULL,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique", "unique_quiet",
    "universal_quiet"),
  .name_spec = NULL,
  .error_call = current_env()
)

vec_cbind(
  ...,
  .ptype = NULL,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal", "unique_quiet",
    "universal_quiet"),
  .error_call = current_env()
)
```

## Arguments

- ...:

  Data frames or vectors.

  When the inputs are named:

  - `vec_rbind()` assigns names to row names unless `.names_to` is
    supplied. In that case the names are assigned in the column defined
    by `.names_to`.

  - `vec_cbind()` creates packed data frame columns with named inputs.

  `NULL` inputs are silently ignored. Empty (e.g. zero row) inputs will
  not appear in the output, but will affect the derived `.ptype`.

- .ptype:

  If `NULL`, the default, the output type is determined by computing the
  common type across all elements of `...`.

  Alternatively, you can supply `.ptype` to give the output known type.
  If `getOption("vctrs.no_guessing")` is `TRUE` you must supply this
  value: this is a convenient way to make production code demand fixed
  types.

- .names_to:

  This controls what to do with names on `...`:

  - By default, names on `...` are
    [zapped](https://rlang.r-lib.org/reference/zap.html) and do not
    appear anywhere in the output.

  - If a string, specifies a column where the names on `...` will be
    copied. These names are often useful to identify rows with their
    original input. If a column name is supplied and `...` is not named,
    an integer column is used instead.

  - If `NULL`, the outer names on `...` are instead merged with inner
    row names on each element of `...` and are subject to `.name_spec`.

- .name_repair:

  One of `"unique"`, `"universal"`, `"check_unique"`, `"unique_quiet"`,
  or `"universal_quiet"`. See
  [`vec_as_names()`](https://vctrs.r-lib.org/dev/reference/vec_as_names.md)
  for the meaning of these options.

  With `vec_rbind()`, the repair function is applied to all inputs
  separately. This is because `vec_rbind()` needs to align their columns
  before binding the rows, and thus needs all inputs to have unique
  names. On the other hand, `vec_cbind()` applies the repair function
  after all inputs have been concatenated together in a final data
  frame. Hence `vec_cbind()` allows the more permissive minimal names
  repair.

- .name_spec:

  A name specification (as documented in
  [`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md)) for
  combining the outer names on `...` with the inner row names of each
  element of `...`. An outer name will only ever be provided when
  `.names_to` is set to `NULL`, which causes the outer name to be used
  as part of the row names rather than as a new column, but it can still
  be useful to hardcode this to either
  [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html) to always
  ignore all names, or `"inner"` to always ignore outer names,
  regardless of `.names_to`.

- .error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- .size:

  If, `NULL`, the default, will determine the number of rows in
  `vec_cbind()` output by using the tidyverse [recycling
  rules](https://vctrs.r-lib.org/dev/reference/theory-faq-recycling.md).

  Alternatively, specify the desired number of rows, and any inputs of
  length 1 will be recycled appropriately.

## Value

A data frame, or subclass of data frame.

If `...` is a mix of different data frame subclasses,
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
will be used to determine the output type. For `vec_rbind()`, this will
determine the type of the container and the type of each column; for
`vec_cbind()` it only determines the type of the output container. If
there are no non-`NULL` inputs, the result will be
[`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Invariants

All inputs are first converted to a data frame. The conversion for 1d
vectors depends on the direction of binding:

- For `vec_rbind()`, each element of the vector becomes a column in a
  single row.

- For `vec_cbind()`, each element of the vector becomes a row in a
  single column.

Once the inputs have all become data frames, the following invariants
are observed for row-binding:

- `vec_size(vec_rbind(x, y)) == vec_size(x) + vec_size(y)`

- `vec_ptype(vec_rbind(x, y)) = vec_ptype_common(x, y)`

Note that if an input is an empty vector, it is first converted to a
1-row data frame with 0 columns. Despite being empty, its effective size
for the total number of rows is 1.

For column-binding, the following invariants apply:

- `vec_size(vec_cbind(x, y)) == vec_size_common(x, y)`

- `vec_ptype(vec_cbind(x, y)) == vec_cbind(vec_ptype(x), vec_ptype(x))`

## Dependencies

### vctrs dependencies

- [`vec_cast_common()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md)

- [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

- [`vec_init()`](https://vctrs.r-lib.org/dev/reference/vec_init.md)

- [`vec_assign()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)

- [`vec_restore()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

### base dependencies of `vec_rbind()`

- [`base::c()`](https://rdrr.io/r/base/c.html)

If columns to combine inherit from a common class, `vec_rbind()` falls
back to [`base::c()`](https://rdrr.io/r/base/c.html) if there exists a
[`c()`](https://rdrr.io/r/base/c.html) method implemented for this class
hierarchy.

## See also

[`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md) for
combining 1d vectors.

## Examples

``` r
# row binding -----------------------------------------

# common columns are coerced to common class
vec_rbind(
  data.frame(x = 1),
  data.frame(x = FALSE)
)
#>   x
#> 1 1
#> 2 0

# unique columns are filled with NAs
vec_rbind(
  data.frame(x = 1),
  data.frame(y = "x")
)
#>    x    y
#> 1  1 <NA>
#> 2 NA    x

# null inputs are ignored
vec_rbind(
  data.frame(x = 1),
  NULL,
  data.frame(x = 2)
)
#>   x
#> 1 1
#> 2 2

# bare vectors are treated as rows
vec_rbind(
  c(x = 1, y = 2),
  c(x = 3)
)
#>   x  y
#> 1 1  2
#> 2 3 NA

# default names will be supplied if arguments are not named
vec_rbind(
  1:2,
  1:3,
  1:4
)
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
#> • `` -> `...4`
#>   ...1 ...2 ...3 ...4
#> 1    1    2   NA   NA
#> 2    1    2    3   NA
#> 3    1    2    3    4

# column binding --------------------------------------

# each input is recycled to have common length
vec_cbind(
  data.frame(x = 1),
  data.frame(y = 1:3)
)
#>   x y
#> 1 1 1
#> 2 1 2
#> 3 1 3

# bare vectors are treated as columns
vec_cbind(
  data.frame(x = 1),
  y = letters[1:3]
)
#>   x y
#> 1 1 a
#> 2 1 b
#> 3 1 c

# if you supply a named data frame, it is packed in a single column
data <- vec_cbind(
  x = data.frame(a = 1, b = 2),
  y = 1
)
data
#>   x.a x.b y
#> 1   1   2 1

# Packed data frames are nested in a single column. This makes it
# possible to access it through a single name:
data$x
#>   a b
#> 1 1 2

# since the base print method is suboptimal with packed data
# frames, it is recommended to use tibble to work with these:
if (rlang::is_installed("tibble")) {
  vec_cbind(x = tibble::tibble(a = 1, b = 2), y = 1)
}
#> # A tibble: 1 × 2
#>     x$a    $b     y
#>   <dbl> <dbl> <dbl>
#> 1     1     2     1

# duplicate names are flagged
vec_cbind(x = 1, x = 2)
#> New names:
#> • `x` -> `x...1`
#> • `x` -> `x...2`
#>   x...1 x...2
#> 1     1     2
```
