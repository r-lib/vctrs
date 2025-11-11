# Number of observations

`vec_size(x)` returns the size of a vector. `vec_is_empty()` returns
`TRUE` if the size is zero, `FALSE` otherwise.

The size is distinct from the
[`length()`](https://rdrr.io/r/base/length.html) of a vector because it
generalises to the "number of observations" for 2d structures, i.e. it's
the number of rows in matrix or a data frame. This definition has the
important property that every column of a data frame (even data frame
and matrix columns) have the same size. `vec_size_common(...)` returns
the common size of multiple vectors.

`list_sizes()` returns an integer vector containing the size of each
element of a list. It is nearly equivalent to, but faster than,
`map_int(x, vec_size)`, with the exception that `list_sizes()` will
error on non-list inputs, as defined by
[`obj_is_list()`](https://vctrs.r-lib.org/dev/reference/obj_is_list.md).
`list_sizes()` is to `vec_size()` as
[`lengths()`](https://rdrr.io/r/base/lengths.html) is to
[`length()`](https://rdrr.io/r/base/length.html).

## Usage

``` r
vec_size(x)

vec_size_common(
  ...,
  .size = NULL,
  .absent = 0L,
  .arg = "",
  .call = caller_env()
)

list_sizes(x)

vec_is_empty(x)
```

## Arguments

- x, ...:

  Vector inputs or `NULL`.

- .size:

  If `NULL`, the default, the output size is determined by recycling the
  lengths of all elements of `...`. Alternatively, you can supply
  `.size` to force a known size; in this case, `x` and `...` are
  ignored.

- .absent:

  The size used when no input is provided, or when all input is `NULL`.
  If left as `NULL` when no input is supplied, an error is thrown.

- .arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- .call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

An integer (or double for long vectors).

`vec_size_common()` returns `.absent` if all inputs are `NULL` or
absent, `0L` by default.

## Details

There is no vctrs helper that retrieves the number of columns: as this
is a property of the
[type](https://vctrs.r-lib.org/dev/reference/vec_ptype.md).

`vec_size()` is equivalent to
[`NROW()`](https://rdrr.io/r/base/nrow.html) but has a name that is
easier to pronounce, and throws an error when passed non-vector inputs.

## Invariants

- `vec_size(dataframe)` == `vec_size(dataframe[[i]])`

- `vec_size(matrix)` == `vec_size(matrix[, i, drop = FALSE])`

- `vec_size(vec_c(x, y))` == `vec_size(x)` + `vec_size(y)`

## The size of NULL

The size of `NULL` is hard-coded to `0L` in `vec_size()`.
`vec_size_common()` returns `.absent` when all inputs are `NULL` (if
only some inputs are `NULL`, they are simply ignored).

A default size of 0 makes sense because sizes are most often queried in
order to compute a total size while assembling a collection of vectors.
Since we treat `NULL` as an absent input by principle, we return the
identity of sizes under addition to reflect that an absent input doesn't
take up any size.

Note that other defaults might make sense under different circumstances.
For instance, a default size of 1 makes sense for finding the common
size because 1 is the identity of the recycling rules.

## Dependencies

- [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

## See also

[`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md) for
a variation of `[` compatible with `vec_size()`, and
[`vec_recycle()`](https://vctrs.r-lib.org/dev/reference/vec_recycle.md)
to
[recycle](https://vctrs.r-lib.org/dev/reference/theory-faq-recycling.md)
vectors to common length.

## Examples

``` r
vec_size(1:100)
#> [1] 100
vec_size(mtcars)
#> [1] 32
vec_size(array(dim = c(3, 5, 10)))
#> [1] 3

vec_size_common(1:10, 1:10)
#> [1] 10
vec_size_common(1:10, 1)
#> [1] 10
vec_size_common(integer(), 1)
#> [1] 0

list_sizes(list("a", 1:5, letters))
#> [1]  1  5 26
```
