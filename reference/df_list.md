# Collect columns for data frame construction

`df_list()` constructs the data structure underlying a data frame, a
named list of equal-length vectors. It is often used in combination with
[`new_data_frame()`](https://vctrs.r-lib.org/reference/new_data_frame.md)
to safely and consistently create a helper function for data frame
subclasses.

## Usage

``` r
df_list(
  ...,
  .size = NULL,
  .unpack = TRUE,
  .name_repair = c("check_unique", "unique", "universal", "minimal", "unique_quiet",
    "universal_quiet"),
  .error_call = current_env()
)
```

## Arguments

- ...:

  Vectors of equal-length. When inputs are named, those names are used
  for names of the resulting list.

- .size:

  The common size of vectors supplied in `...`. If `NULL`, this will be
  computed as the common size of the inputs.

- .unpack:

  Should unnamed data frame inputs be unpacked? Defaults to `TRUE`.

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

[`new_data_frame()`](https://vctrs.r-lib.org/reference/new_data_frame.md)
for constructing data frame subclasses from a validated input.
[`data_frame()`](https://vctrs.r-lib.org/reference/data_frame.md) for a
fast data frame creation helper.

## Examples

``` r
# `new_data_frame()` can be used to create custom data frame constructors
new_fancy_df <- function(x = list(), n = NULL, ..., class = NULL) {
  new_data_frame(x, n = n, ..., class = c(class, "fancy_df"))
}

# Combine this constructor with `df_list()` to create a safe,
# consistent helper function for your data frame subclass
fancy_df <- function(...) {
  data <- df_list(...)
  new_fancy_df(data)
}

df <- fancy_df(x = 1)
class(df)
#> [1] "fancy_df"   "data.frame"
```
