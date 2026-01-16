# Construct a data frame

`data_frame()` constructs a data frame. It is similar to
[`base::data.frame()`](https://rdrr.io/r/base/data.frame.html), but
there are a few notable differences that make it more in line with vctrs
principles. The Properties section outlines these.

## Usage

``` r
data_frame(
  ...,
  .size = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal", "unique_quiet",
    "universal_quiet"),
  .error_call = current_env()
)
```

## Arguments

- ...:

  Vectors to become columns in the data frame. When inputs are named,
  those names are used for column names.

- .size:

  The number of rows in the data frame. If `NULL`, this will be computed
  as the common size of the inputs.

- .name_repair:

  One of `"check_unique"`, `"unique"`, `"universal"`, `"minimal"`,
  `"unique_quiet"`, or `"universal_quiet"`. See
  [`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.md)
  for the meaning of these options.

- .error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Details

If no column names are supplied, `""` will be used as a default name for
all columns. This is applied before name repair occurs, so the default
name repair of `"check_unique"` will error if any unnamed inputs are
supplied and `"unique"` (or `"unique_quiet"`) will repair the empty
string column names appropriately. If the column names don't matter, use
a `"minimal"` name repair for convenience and performance.

## Properties

- Inputs are
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.md)
  to a common size with
  [`vec_recycle_common()`](https://vctrs.r-lib.org/reference/vec_recycle.md).

- With the exception of data frames, inputs are not modified in any way.
  Character vectors are never converted to factors, and lists are stored
  as-is for easy creation of list-columns.

- Unnamed data frame inputs are automatically unpacked. Named data frame
  inputs are stored unmodified as data frame columns.

- `NULL` inputs are completely ignored.

- The dots are dynamic, allowing for splicing of lists with `!!!` and
  unquoting.

## See also

[`df_list()`](https://vctrs.r-lib.org/reference/df_list.md) for safely
creating a data frame's underlying data structure from individual
columns.
[`new_data_frame()`](https://vctrs.r-lib.org/reference/new_data_frame.md)
for constructing the actual data frame from that underlying data
structure. Together, these can be useful for developers when creating
new data frame subclasses supporting standard evaluation.

## Examples

``` r
data_frame(x = 1, y = 2)
#>   x y
#> 1 1 2

# Inputs are recycled using tidyverse recycling rules
data_frame(x = 1, y = 1:3)
#>   x y
#> 1 1 1
#> 2 1 2
#> 3 1 3

# Strings are never converted to factors
class(data_frame(x = "foo")$x)
#> [1] "character"

# List columns can be easily created
df <- data_frame(x = list(1:2, 2, 3:4), y = 3:1)

# However, the base print method is suboptimal for displaying them,
# so it is recommended to convert them to tibble
if (rlang::is_installed("tibble")) {
  tibble::as_tibble(df)
}
#> # A tibble: 3 × 2
#>   x             y
#>   <list>    <int>
#> 1 <int [2]>     3
#> 2 <dbl [1]>     2
#> 3 <int [2]>     1

# Named data frame inputs create data frame columns
df <- data_frame(x = data_frame(y = 1:2, z = "a"))

# The `x` column itself is another data frame
df$x
#>   y z
#> 1 1 a
#> 2 2 a

# Again, it is recommended to convert these to tibbles for a better
# print method
if (rlang::is_installed("tibble")) {
  tibble::as_tibble(df)
}
#> # A tibble: 2 × 1
#>     x$y $z   
#>   <int> <chr>
#> 1     1 a    
#> 2     2 a    

# Unnamed data frame input is automatically unpacked
data_frame(x = 1, data_frame(y = 1:2, z = "a"))
#>   x y z
#> 1 1 1 a
#> 2 1 2 a
```
