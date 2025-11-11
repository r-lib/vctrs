# FAQ - Is my class compatible with vctrs?

vctrs provides a framework for working with vector classes in a generic
way. However, it implements several compatibility fallbacks to base R
methods. In this reference you will find how vctrs tries to be
compatible with your vector class, and what base methods you need to
implement for compatibility.

If you’re starting from scratch, we think you’ll find it easier to start
using [`new_vctr()`](https://vctrs.r-lib.org/dev/reference/new_vctr.md)
as documented in
[`vignette("s3-vector")`](https://vctrs.r-lib.org/dev/articles/s3-vector.md).
This guide is aimed for developers with existing vector classes.

### Aggregate operations with fallbacks

All vctrs operations are based on four primitive generics described in
the next section. However there are many higher level operations. The
most important ones implement fallbacks to base generics for maximum
compatibility with existing classes.

- [`vec_slice()`](https://vctrs.r-lib.org/dev/reference/vec_slice.md)
  falls back to the base `[` generic if no
  [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)
  method is implemented. This way foreign classes that do not implement
  [`vec_restore()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)
  can restore attributes based on the new subsetted contents.

- [`vec_c()`](https://vctrs.r-lib.org/dev/reference/vec_c.md) and
  [`vec_rbind()`](https://vctrs.r-lib.org/dev/reference/vec_bind.md) now
  fall back to [`base::c()`](https://rdrr.io/r/base/c.html) if the
  inputs have a common parent class with a
  [`c()`](https://rdrr.io/r/base/c.html) method (only if they have no
  self-to-self
  [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
  method).

  vctrs works hard to make your [`c()`](https://rdrr.io/r/base/c.html)
  method success in various situations (with `NULL` and `NA` inputs,
  even as first input which would normally prevent dispatch to your
  method). The main downside compared to using vctrs primitives is that
  you can’t combine vectors of different classes since there is no
  extensible mechanism of coercion in
  [`c()`](https://rdrr.io/r/base/c.html), and it is less efficient in
  some cases.

### The vctrs primitives

Most functions in vctrs are aggregate operations: they call other vctrs
functions which themselves call other vctrs functions. The dependencies
of a vctrs functions are listed in the Dependencies section of its
documentation page. Take a look at
[`vec_count()`](https://vctrs.r-lib.org/dev/reference/vec_count.md) for
an example.

These dependencies form a tree whose leaves are the four vctrs
primitives. Here is the diagram for
[`vec_count()`](https://vctrs.r-lib.org/dev/reference/vec_count.md):

![](figures/vec-count-deps.png)

#### The coercion generics

The coercion mechanism in vctrs is based on two generics:

- [`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)

- [`vec_cast()`](https://vctrs.r-lib.org/dev/reference/vec_cast.md)

See the [theory
overview](https://vctrs.r-lib.org/dev/reference/theory-faq-coercion.md).

Two objects with the same class and the same attributes are always
considered compatible by ptype2 and cast. If the attributes or classes
differ, they throw an incompatible type error.

Coercion errors are the main source of incompatibility with vctrs. See
the [howto
guide](https://vctrs.r-lib.org/dev/reference/howto-faq-coercion.md) if
you need to implement methods for these generics.

#### The proxy and restoration generics

- [`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

- [`vec_restore()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)

These generics are essential for vctrs but mostly optional.
[`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)
defaults to an [identity](https://rdrr.io/r/base/identity.html) function
and you normally don’t need to implement it. The proxy a vector must be
one of the atomic vector types, a list, or a data frame. By default, S3
lists that do not inherit from `"list"` do not have an identity proxy.
In that case, you need to explicitly implement
[`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md) or
make your class inherit from list.
