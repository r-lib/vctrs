# Proxy and restore

**\[experimental\]**

`vec_proxy()` returns the data structure containing the values of a
vector. This data structure is usually the vector itself. In this case
the proxy is the [identity
function](https://rdrr.io/r/base/identity.html), which is the default
`vec_proxy()` method.

Only experts should implement special `vec_proxy()` methods, for these
cases:

- A vector has vectorised attributes, i.e. metadata for each element of
  the vector. These *record types* are implemented in vctrs by returning
  a data frame in the proxy method. If you're starting your class from
  scratch, consider deriving from the
  [`rcrd`](https://vctrs.r-lib.org/reference/new_rcrd.md) class. It
  implements the appropriate data frame proxy and is generally the
  preferred way to create a record class.

- When you're implementing a vector on top of a non-vector type, like an
  environment or an S4 object. This is currently only partially
  supported.

- S3 lists are considered scalars by default. This is the safe choice
  for list objects such as returned by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html). To declare that your
  S3 list class is a vector, you normally add `"list"` to the right of
  your class vector. Explicit inheritance from list is generally the
  preferred way to declare an S3 list in R, for instance it makes it
  possible to dispatch on `generic.list` S3 methods.

  If you can't modify your class vector, you can implement an identity
  proxy (i.e. a proxy method that just returns its input) to let vctrs
  know this is a vector list and not a scalar.

`vec_restore()` is the inverse operation of `vec_proxy()`. It should
only be called on vector proxies.

- It undoes the transformations of `vec_proxy()`.

- It restores attributes and classes. These may be lost when the memory
  values are manipulated. For example slicing a subset of a vector's
  proxy causes a new proxy to be allocated.

By default vctrs restores all attributes and classes automatically. You
only need to implement a `vec_restore()` method if your class has
attributes that depend on the data.

## Usage

``` r
vec_proxy(x, ...)

vec_restore(x, to, ...)
```

## Arguments

- x:

  A vector.

- ...:

  These dots are for future extensions and must be empty.

- to:

  The original vector to restore to.

## Proxying

You should only implement `vec_proxy()` when your type is designed
around a non-vector class. I.e. anything that is not either:

- An atomic vector

- A bare list

- A data frame

In this case, implement `vec_proxy()` to return such a vector class. The
vctrs operations such as
[`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md) are
applied on the proxy and `vec_restore()` is called to restore the
original representation of your type.

The most common case where you need to implement `vec_proxy()` is for S3
lists. In vctrs, S3 lists are treated as scalars by default. This way we
don't treat objects like model fits as vectors. To prevent vctrs from
treating your S3 list as a scalar, unclass it in the `vec_proxy()`
method. For instance, here is the definition for `list_of`:

    vec_proxy.vctrs_list_of <- function(x) {
      unclass(x)
    }

Another case where you need to implement a proxy is [record
types](https://vctrs.r-lib.org/reference/new_rcrd.md). Record types
should return a data frame, as in the `POSIXlt` method:

    vec_proxy.POSIXlt <- function(x) {
      new_data_frame(unclass(x))
    }

Note that you don't need to implement `vec_proxy()` when your class
inherits from `vctrs_vctr` or `vctrs_rcrd`.

## Restoring

A restore is a specialised type of cast, primarily used in conjunction
with [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) or a
C-level function that works on the underlying data structure. A
`vec_restore()` method can make the following assumptions about `x`:

- It has the correct type.

- It has the correct names.

- It has the correct `dim` and `dimnames` attributes.

- It is unclassed. This way you can call vctrs generics with `x` without
  triggering an infinite loop of restoration.

The length may be different (for example after
[`vec_slice()`](https://vctrs.r-lib.org/reference/vec_slice.md) has been
called), and all other attributes may have been lost. The method should
restore all attributes so that after restoration,
`vec_restore(vec_data(x), x)` yields `x`.

To understand the difference between
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) and
`vec_restore()` think about factors: it doesn't make sense to cast an
integer to a factor, but if
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) or another
low-level function has stripped attributes, you still need to be able to
restore them.

The default method copies across all attributes so you only need to
provide your own method if your attributes require special care (i.e.
they are dependent on the data in some way). When implementing your own
method, bear in mind that many R users add attributes to track
additional metadata that is important to them, so you should preserve
any attributes that don't require special handling for your class.

## Dependencies

- `x` must be a vector in the vctrs sense (see
  [`vec_is()`](https://vctrs.r-lib.org/reference/vec_assert.md))

- By default the underlying data is returned as is (identity proxy)

All vector classes have a proxy, even those who don't implement any
vctrs methods. The exception is S3 lists that don't inherit from
`"list"` explicitly. These might have to implement an identity proxy for
compatibility with vctrs (see discussion above).
