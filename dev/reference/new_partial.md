# Partial type

**\[experimental\]**

Use `new_partial()` when constructing a new partial type subclass; and
use `is_partial()` to test if a type is partial. All subclasses need to
provide a `vec_ptype_finalise()` method.

## Usage

``` r
new_partial(..., class = character())

is_partial(x)

vec_ptype_finalise(x, ...)
```

## Arguments

- ...:

  Attributes of the partial type

- class:

  Name of subclass.

## Details

As the name suggests, a partial type *partially* specifies a type, and
it must be combined with data to yield a full type. A useful example of
a partial type is
[`partial_frame()`](https://vctrs.r-lib.org/dev/reference/partial_frame.md),
which makes it possible to specify the type of just a few columns in a
data frame. Use this constructor if you're making your own partial type.
