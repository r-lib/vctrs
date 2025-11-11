# Internal FAQ - `vec_ptype2()`, `NULL`, and unspecified vectors

### Promotion monoid

Promotions (i.e. automatic coercions) should always transform inputs to
their richer type to avoid losing values of precision.
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
returns the *richer* type of two vectors, or throws an incompatible type
error if none of the two vector types include the other. For example,
the richer type of integer and double is the latter because double
covers a larger range of values than integer.

[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md) is
a [monoid](https://en.wikipedia.org/wiki/Monoid) over vectors, which in
practical terms means that it is a well behaved operation for
[reduction](https://purrr.tidyverse.org/reference/reduce.html).
Reduction is an important operation for promotions because that is how
the richer type of multiple elements is computed. As a monoid,
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
needs an identity element, i.e. a value that doesn’t change the result
of the reduction. vctrs has two identity values, `NULL` and
**unspecified** vectors.

### The `NULL` identity

As an identity element that shouldn’t influence the determination of the
common type of a set of vectors, `NULL` is promoted to any type:

    vec_ptype2(NULL, "")
    #> character(0)
    vec_ptype2(1L, NULL)
    #> integer(0)

The common type of `NULL` and `NULL` is the identity `NULL`:

    vec_ptype2(NULL, NULL)
    #> NULL

This way the result of `vec_ptype2(NULL, NULL)` does not influence
subsequent promotions:

    vec_ptype2(
      vec_ptype2(NULL, NULL),
      ""
    )
    #> character(0)

### Unspecified vectors

In the vctrs coercion system, logical vectors of missing values are also
automatically promoted to the type of any other vector, just like
`NULL`. We call these vectors unspecified. The special coercion
semantics of unspecified vectors serve two purposes:

1.  It makes it possible to assign vectors of `NA` inside any type of
    vectors, even when they are not coercible with logical:

        x <- letters[1:5]
        vec_assign(x, 1:2, c(NA, NA))
        #> [1] NA  NA  "c" "d" "e"

2.  We can’t put `NULL` in a data frame, so we need an identity element
    that behaves more like a vector. Logical vectors of `NA` seem a
    natural fit for this.

Unspecified vectors are thus promoted to any other type, just like
`NULL`:

    vec_ptype2(NA, "")
    #> character(0)
    vec_ptype2(1L, c(NA, NA))
    #> integer(0)

### Finalising common types

vctrs has an internal vector type of class `vctrs_unspecified`. Users
normally don’t see such vectors in the wild, but they do come up when
taking the common type of an unspecified vector with another identity
value:

    vec_ptype2(NA, NA)
    #> <unspecified> [0]
    vec_ptype2(NA, NULL)
    #> <unspecified> [0]
    vec_ptype2(NULL, NA)
    #> <unspecified> [0]

We can’t return `NA` here because
[`vec_ptype2()`](https://vctrs.r-lib.org/dev/reference/vec_ptype2.md)
normally returns empty vectors. We also can’t return `NULL` because
unspecified vectors need to be recognised as logical vectors if they
haven’t been promoted at the end of the reduction.

    vec_ptype_finalise(vec_ptype2(NULL, NA))
    #> logical(0)

See the output of
[`vec_ptype_common()`](https://vctrs.r-lib.org/dev/reference/vec_ptype.md)
which performs the reduction and finalises the type, ready to be used by
the caller:

    vec_ptype_common(NULL, NULL)
    #> NULL
    vec_ptype_common(NA, NULL)
    #> logical(0)

Note that **partial** types in vctrs make use of the same mechanism.
They are finalised with
[`vec_ptype_finalise()`](https://vctrs.r-lib.org/dev/reference/new_partial.md).
