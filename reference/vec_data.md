# Extract underlying data

**\[experimental\]**

Extract the data underlying an S3 vector object, i.e. the underlying
(named) atomic vector, data frame, or list.

## Usage

``` r
vec_data(x)
```

## Arguments

- x:

  A vector or object implementing
  [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md).

## Value

The data underlying `x`, free from any attributes except the names.

## Difference with [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)

- `vec_data()` returns unstructured data. The only attributes preserved
  are names, dims, and dimnames.

  Currently, due to the underlying memory architecture of R, this
  creates a full copy of the data for atomic vectors.

- [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md) may
  return structured data. This generic is the main customisation point
  for accessing memory values in vctrs, along with
  [`vec_restore()`](https://vctrs.r-lib.org/reference/vec_proxy.md).

  Methods must return a vector type. Records and data frames will be
  processed rowwise.
