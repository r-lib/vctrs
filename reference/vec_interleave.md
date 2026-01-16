# Interleave many vectors into one vector

`vec_interleave()` combines multiple vectors together, much like
[`vec_c()`](https://vctrs.r-lib.org/reference/vec_c.md), but does so in
such a way that the elements of each vector are interleaved together.

It is a more efficient equivalent to the following usage of
[`vec_c()`](https://vctrs.r-lib.org/reference/vec_c.md):

    vec_interleave(x, y) == vec_c(x[1], y[1], x[2], y[2], ..., x[n], y[n])

## Usage

``` r
vec_interleave(
  ...,
  .size = NULL,
  .ptype = NULL,
  .name_spec = NULL,
  .name_repair = c("minimal", "unique", "check_unique", "universal", "unique_quiet",
    "universal_quiet"),
  .error_call = current_env()
)
```

## Arguments

- ...:

  Vectors to interleave.

- .size:

  The expected size of each vector. If not provided, computed
  automatically by
  [`vec_size_common()`](https://vctrs.r-lib.org/reference/vec_size.md).
  Each vector will be
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.md)
  to this size.

- .ptype:

  The expected type of each vector. If not provided, computed
  automatically by
  [`vec_ptype_common()`](https://vctrs.r-lib.org/reference/vec_ptype.md).
  Each vector will be
  [cast](https://vctrs.r-lib.org/reference/theory-faq-coercion.md) to
  this type.

- .name_spec:

  A name specification for combining inner and outer names. This is
  relevant for inputs passed with a name, when these inputs are
  themselves named, like `outer = c(inner = 1)`, or when they have
  length greater than 1: `outer = 1:2`. By default, these cases trigger
  an error. You can resolve the error by providing a specification that
  describes how to combine the names or the indices of the inner vector
  with the name of the input. This specification can be:

  - A function of two arguments. The outer name is passed as a string to
    the first argument, and the inner names or positions are passed as
    second argument.

  - An anonymous function as a purrr-style formula.

  - A glue specification of the form `"{outer}_{inner}"`.

  - `"inner"`, in which case outer names are ignored, and inner names
    are used if they exist. Note that outer names may still be used to
    provide informative error messages.

  - An [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html)
    object, in which case both outer and inner names are ignored and the
    result is unnamed.

  See the [name specification
  topic](https://vctrs.r-lib.org/reference/name_spec.md).

- .name_repair:

  How to repair names, see `repair` options in
  [`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.md).

- .error_call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Dependencies

### vctrs dependencies

- [`list_combine()`](https://vctrs.r-lib.org/reference/list_combine.md)

## Examples

``` r
# The most common case is to interleave two vectors
vec_interleave(1:3, 4:6)
#> [1] 1 4 2 5 3 6

# But you aren't restricted to just two
vec_interleave(1:3, 4:6, 7:9, 10:12)
#>  [1]  1  4  7 10  2  5  8 11  3  6  9 12

# You can also interleave data frames
x <- data_frame(x = 1:2, y = c("a", "b"))
y <- data_frame(x = 3:4, y = c("c", "d"))

vec_interleave(x, y)
#> # A tibble: 4 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     3 c    
#> 3     2 b    
#> 4     4 d    

# `.size` can be used to recycle size 1 elements before interleaving
vec_interleave(1, 2, .size = 3)
#> [1] 1 2 1 2 1 2

# `.ptype` can be used to enforce a particular type
typeof(vec_interleave(1, 2, .ptype = integer()))
#> [1] "integer"
```
