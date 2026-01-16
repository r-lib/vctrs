# FAQ - How to implement ptype2 and cast methods?

This guide illustrates how to implement
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) and
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) methods
for existing classes. Related topics:

- For an overview of how these generics work and their roles in vctrs,
  see
  [`?theory-faq-coercion`](https://vctrs.r-lib.org/reference/theory-faq-coercion.md).

- For an example of implementing coercion methods for data frame
  subclasses, see
  [`?howto-faq-coercion-data-frame`](https://vctrs.r-lib.org/reference/howto-faq-coercion-data-frame.md).

- For a tutorial about implementing vctrs classes from scratch, see
  [`vignette("s3-vector")`](https://vctrs.r-lib.org/articles/s3-vector.md)

### The natural number class

We’ll illustrate how to implement coercion methods with a simple class
that represents natural numbers. In this scenario we have an existing
class that already features a constructor and methods for
[`print()`](https://rdrr.io/r/base/print.html) and subset.

    #' @export
    new_natural <- function(x) {
      if (is.numeric(x) || is.logical(x)) {
        stopifnot(is_whole(x))
        x <- as.integer(x)
      } else {
        stop("Can't construct natural from unknown type.")
      }
      structure(x, class = "my_natural")
    }
    is_whole <- function(x) {
      all(x %% 1 == 0 | is.na(x))
    }

    #' @export
    print.my_natural <- function(x, ...) {
      cat("<natural>\n")
      x <- unclass(x)
      NextMethod()
    }
    #' @export
    `[.my_natural` <- function(x, i, ...) {
      new_natural(NextMethod())
    }

    new_natural(1:3)
    #> <natural>
    #> [1] 1 2 3
    new_natural(c(1, NA))
    #> <natural>
    #> [1]  1 NA

### Roxygen workflow

To implement methods for generics, first import the generics in your
namespace and redocument:

    #' @importFrom vctrs vec_ptype2 vec_cast
    NULL

Note that for each batches of methods that you add to your package, you
need to export the methods and redocument immediately, even during
development. Otherwise they won’t be in scope when you run unit tests
e.g. with testthat.

Implementing double dispatch methods is very similar to implementing
regular S3 methods. In these examples we are using roxygen2 tags to
register the methods, but you can also register the methods manually in
your NAMESPACE file or lazily with
[`s3_register()`](https://vctrs.r-lib.org/reference/s3_register.md).

### Implementing [`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)

#### The self-self method

The first method to implement is the one that signals that your class is
compatible with itself:

    #' @export
    vec_ptype2.my_natural.my_natural <- function(x, y, ...) {
      x
    }

    vec_ptype2(new_natural(1), new_natural(2:3))
    #> <natural>
    #> integer(0)

[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)
implements a fallback to try and be compatible with simple classes, so
it may seem that you don’t need to implement the self-self coercion
method. However, you must implement it explicitly because this is how
vctrs knows that a class that is implementing vctrs methods (for
instance this disable fallbacks to
[`base::c()`](https://rdrr.io/r/base/c.html)). Also, it makes your class
a bit more efficient.

#### The parent and children methods

Our natural number class is conceptually a parent of `<logical>` and a
child of `<integer>`, but the class is not compatible with logical,
integer, or double vectors yet:

    vec_ptype2(TRUE, new_natural(2:3))
    #> Error:
    #> ! Can't combine `TRUE` <logical> and `new_natural(2:3)` <my_natural>.

    vec_ptype2(new_natural(1), 2:3)
    #> Error:
    #> ! Can't combine `new_natural(1)` <my_natural> and `2:3` <integer>.

We’ll specify the twin methods for each of these classes, returning the
richer class in each case.

    #' @export
    vec_ptype2.my_natural.logical <- function(x, y, ...) {
      # The order of the classes in the method name follows the order of
      # the arguments in the function signature, so `x` is the natural
      # number and `y` is the logical
      x
    }
    #' @export
    vec_ptype2.logical.my_natural <- function(x, y, ...) {
      # In this case `y` is the richer natural number
      y
    }

Between a natural number and an integer, the latter is the richer class:

    #' @export
    vec_ptype2.my_natural.integer <- function(x, y, ...) {
      y
    }
    #' @export
    vec_ptype2.integer.my_natural <- function(x, y, ...) {
      x
    }

We no longer get common type errors for logical and integer:

    vec_ptype2(TRUE, new_natural(2:3))
    #> <natural>
    #> integer(0)

    vec_ptype2(new_natural(1), 2:3)
    #> integer(0)

We are not done yet. Pairwise coercion methods must be implemented for
all the connected nodes in the coercion hierarchy, which include double
vectors further up. The coercion methods for grand-parent types must be
implemented separately:

    #' @export
    vec_ptype2.my_natural.double <- function(x, y, ...) {
      y
    }
    #' @export
    vec_ptype2.double.my_natural <- function(x, y, ...) {
      x
    }

#### Incompatible attributes

Most of the time, inputs are incompatible because they have different
classes for which no
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) method
is implemented. More rarely, inputs could be incompatible because of
their attributes. In that case incompatibility is signalled by calling
[`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md).

In the following example, we implement a self-self ptype2 method for a
hypothetical subclass of `<factor>` that has stricter combination
semantics. The method throws an error when the levels of the two factors
are not compatible.

    #' @export
    vec_ptype2.my_strict_factor.my_strict_factor <- function(x, y, ..., x_arg = "", y_arg = "") {
      if (!setequal(levels(x), levels(y))) {
        stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
      }

      x
    }

Note how the methods need to take `x_arg` and `y_arg` parameters and
pass them on to
[`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md).
These argument tags help create more informative error messages when the
common type determination is for a column of a data frame. They are part
of the generic signature but can usually be left out if not used.

### Implementing [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)

Corresponding
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) methods
must be implemented for all
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)
methods. The general pattern is to convert the argument `x` to the type
of `to`. The methods should validate the values in `x` and make sure
they conform to the values of `to`.

Please note that for historical reasons, the order of the classes in the
method name is in reverse order of the arguments in the function
signature. The first class represents `to`, whereas the second class
represents `x`.

The self-self method is easy in this case, it just returns the target
input:

    #' @export
    vec_cast.my_natural.my_natural <- function(x, to, ...) {
      x
    }

The other types need to be validated. We perform input validation in the
`new_natural()` constructor, so that’s a good fit for our
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)
implementations.

    #' @export
    vec_cast.my_natural.logical <- function(x, to, ...) {
      # The order of the classes in the method name is in reverse order
      # of the arguments in the function signature, so `to` is the natural
      # number and `x` is the logical
      new_natural(x)
    }
    vec_cast.my_natural.integer <- function(x, to, ...) {
      new_natural(x)
    }
    vec_cast.my_natural.double <- function(x, to, ...) {
      new_natural(x)
    }

With these methods, vctrs is now able to combine logical and natural
vectors. It properly returns the richer type of the two, a natural
vector:

    vec_c(TRUE, new_natural(1), FALSE)
    #> <natural>
    #> [1] 1 1 0

Because we haven’t implemented conversions *from* natural, it still
doesn’t know how to combine natural with the richer integer and double
types:

    vec_c(new_natural(1), 10L)
    #> Error in `vec_c()`:
    #> ! Can't convert `..1` <my_natural> to <integer>.
    vec_c(1.5, new_natural(1))
    #> Error in `vec_c()`:
    #> ! Can't convert `..2` <my_natural> to <double>.

This is quick work which completes the implementation of coercion
methods for vctrs:

    #' @export
    vec_cast.logical.my_natural <- function(x, to, ...) {
      # In this case `to` is the logical and `x` is the natural number
      attributes(x) <- NULL
      as.logical(x)
    }
    #' @export
    vec_cast.integer.my_natural <- function(x, to, ...) {
      attributes(x) <- NULL
      as.integer(x)
    }
    #' @export
    vec_cast.double.my_natural <- function(x, to, ...) {
      attributes(x) <- NULL
      as.double(x)
    }

And we now get the expected combinations.

    vec_c(new_natural(1), 10L)
    #> [1]  1 10

    vec_c(1.5, new_natural(1))
    #> [1] 1.5 1.0
