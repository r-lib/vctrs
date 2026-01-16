# Factor/ordered factor S3 class

A [factor](https://rdrr.io/r/base/factor.html) is an integer with
attribute `levels`, a character vector. There should be one level for
each integer between 1 and `max(x)`. An
[ordered](https://rdrr.io/r/base/factor.html) factor has the same
properties as a factor, but possesses an extra class that marks levels
as having a total ordering.

## Usage

``` r
new_factor(x = integer(), levels = character(), ..., class = character())

new_ordered(x = integer(), levels = character())

# S3 method for class 'factor'
vec_ptype2(x, y, ...)

# S3 method for class 'ordered'
vec_ptype2(x, y, ...)

# S3 method for class 'factor'
vec_cast(x, to, ...)

# S3 method for class 'ordered'
vec_cast(x, to, ...)
```

## Arguments

- x:

  Integer values which index in to `levels`.

- levels:

  Character vector of labels.

- ..., class:

  Used to for subclasses.

## Details

These functions help the base factor and ordered factor classes fit in
to the vctrs type system by providing constructors, coercion functions,
and casting functions. `new_factor()` and `new_ordered()` are low-level
constructors - they only check that types, but not values, are valid, so
are for expert use only.
