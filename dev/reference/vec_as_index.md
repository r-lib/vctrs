# Convert to an index vector

**\[deprecated\]**

`vec_as_index()` has been renamed to
[`vec_as_location()`](https://vctrs.r-lib.org/dev/reference/vec_as_location.md)
and is deprecated as of vctrs 0.2.2.

## Usage

``` r
vec_as_index(i, n, names = NULL)
```

## Arguments

- i:

  An index vector to convert.

- n:

  A single integer representing the total size of the object that `i` is
  meant to index into.

- names:

  If `i` is a character vector, `names` should be a character vector
  that `i` will be matched against to construct the index. Otherwise,
  not used. The default value of `NULL` will result in an error if `i`
  is a character vector.
