# FAQ - How to implement ptype2 and cast methods? (Data frames)

This guide provides a practical recipe for implementing
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) and
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) methods
for coercions of data frame subclasses. Related topics:

- For an overview of the coercion mechanism in vctrs, see
  [`?theory-faq-coercion`](https://vctrs.r-lib.org/reference/theory-faq-coercion.md).

- For an example of implementing coercion methods for simple vectors,
  see
  [`?howto-faq-coercion`](https://vctrs.r-lib.org/reference/howto-faq-coercion.md).

Coercion of data frames occurs when different data frame classes are
combined in some way. The two main methods of combination are currently
row-binding with
[`vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.md) and
col-binding with
[`vec_cbind()`](https://vctrs.r-lib.org/reference/vec_bind.md) (which
are in turn used by a number of dplyr and tidyr functions). These
functions take multiple data frame inputs and automatically coerce them
to their common type.

vctrs is generally strict about the kind of automatic coercions that are
performed when combining inputs. In the case of data frames we have
decided to be a bit less strict for convenience. Instead of throwing an
incompatible type error, we fall back to a base data frame or a tibble
if we don’t know how to combine two data frame subclasses. It is still a
good idea to specify the proper coercion behaviour for your data frame
subclasses as soon as possible.

We will see two examples in this guide. The first example is about a
data frame subclass that has no particular attributes to manage. In the
second example, we implement coercion methods for a tibble subclass that
includes potentially incompatible attributes.

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

### Parent methods

Most of the common type determination should be performed by the parent
class. In vctrs, double dispatch is implemented in such a way that you
need to call the methods for the parent class manually. For
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) this
means you need to call
[`df_ptype2()`](https://vctrs.r-lib.org/reference/df_ptype2.md) (for
data frame subclasses) or
[`tib_ptype2()`](https://vctrs.r-lib.org/reference/df_ptype2.md) (for
tibble subclasses). Similarly,
[`df_cast()`](https://vctrs.r-lib.org/reference/df_ptype2.md) and
[`tib_cast()`](https://vctrs.r-lib.org/reference/df_ptype2.md) are the
workhorses for
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) methods of
subtypes of `data.frame` and `tbl_df`. These functions take the union of
the columns in `x` and `y`, and ensure shared columns have the same
type.

These functions are much less strict than
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md) and
[`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md) as they
accept any subclass of data frame as input. They always return a
`data.frame` or a `tbl_df`. You will probably want to write similar
functions for your subclass to avoid repetition in your code. You may
want to export them as well if you are expecting other people to derive
from your class.

### A `data.table` example

This example is the actual implementation of vctrs coercion methods for
`data.table`. This is a simple example because we don’t have to keep
track of attributes for this class or manage incompatibilities. See the
tibble section for a more complicated example.

We first create the `dt_ptype2()` and `dt_cast()` helpers. They wrap
around the parent methods
[`df_ptype2()`](https://vctrs.r-lib.org/reference/df_ptype2.md) and
[`df_cast()`](https://vctrs.r-lib.org/reference/df_ptype2.md), and
transform the common type or converted input to a data table. You may
want to export these helpers if you expect other packages to derive from
your data frame class.

These helpers should always return data tables. To this end we use the
conversion generic `as.data.table()`. Depending on the tools available
for the particular class at hand, a constructor might be appropriate as
well.

    dt_ptype2 <- function(x, y, ...) {
      as.data.table(df_ptype2(x, y, ...))
    }
    dt_cast <- function(x, to, ...) {
      as.data.table(df_cast(x, to, ...))
    }

We start with the self-self method:

    #' @export
    vec_ptype2.data.table.data.table <- function(x, y, ...) {
      dt_ptype2(x, y, ...)
    }

Between a data frame and a data table, we consider the richer type to be
data table. This decision is not based on the value coverage of each
data structures, but on the idea that data tables have richer behaviour.
Since data tables are the richer type, we call `dt_type2()` from the
[`vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.md)
method. It always returns a data table, no matter the order of
arguments:

    #' @export
    vec_ptype2.data.table.data.frame <- function(x, y, ...) {
      dt_ptype2(x, y, ...)
    }
    #' @export
    vec_ptype2.data.frame.data.table <- function(x, y, ...) {
      dt_ptype2(x, y, ...)
    }

The [`vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.md)
methods follow the same pattern, but note how the method for coercing to
data frame uses
[`df_cast()`](https://vctrs.r-lib.org/reference/df_ptype2.md) rather
than `dt_cast()`.

Also, please note that for historical reasons, the order of the classes
in the method name is in reverse order of the arguments in the function
signature. The first class represents `to`, whereas the second class
represents `x`.

    #' @export
    vec_cast.data.table.data.table <- function(x, to, ...) {
      dt_cast(x, to, ...)
    }
    #' @export
    vec_cast.data.table.data.frame <- function(x, to, ...) {
      # `x` is a data.frame to be converted to a data.table
      dt_cast(x, to, ...)
    }
    #' @export
    vec_cast.data.frame.data.table <- function(x, to, ...) {
      # `x` is a data.table to be converted to a data.frame
      df_cast(x, to, ...)
    }

With these methods vctrs is now able to combine data tables with data
frames:

    vec_cbind(data.frame(x = 1:3), data.table(y = "foo"))
    #>        x      y
    #>    <int> <char>
    #> 1:     1    foo
    #> 2:     2    foo
    #> 3:     3    foo

### A tibble example

In this example we implement coercion methods for a tibble subclass that
carries a colour as a scalar metadata:

    # User constructor
    my_tibble <- function(colour = NULL, ...) {
      new_my_tibble(tibble::tibble(...), colour = colour)
    }
    # Developer constructor
    new_my_tibble <- function(x, colour = NULL) {
      stopifnot(is.data.frame(x))
      tibble::new_tibble(
        x,
        colour = colour,
        class = "my_tibble",
        nrow = nrow(x)
      )
    }

    df_colour <- function(x) {
      if (inherits(x, "my_tibble")) {
        attr(x, "colour")
      } else {
        NULL
      }
    }

    #'@export
    print.my_tibble <- function(x, ...) {
      cat(sprintf("<%s: %s>\n", class(x)[[1]], df_colour(x)))
      cli::cat_line(format(x)[-1])
    }

This subclass is very simple. All it does is modify the header.

    red <- my_tibble("red", x = 1, y = 1:2)
    red
    #> <my_tibble: red>
    #>       x     y
    #>   <dbl> <int>
    #> 1     1     1
    #> 2     1     2

    red[2]
    #> <my_tibble: red>
    #>       y
    #>   <int>
    #> 1     1
    #> 2     2

    green <- my_tibble("green", z = TRUE)
    green
    #> <my_tibble: green>
    #>   z
    #>   <lgl>
    #> 1 TRUE

Combinations do not work properly out of the box, instead vctrs falls
back to a bare tibble:

    vec_rbind(red, tibble::tibble(x = 10:12))
    #> # A tibble: 5 x 2
    #>       x     y
    #>   <dbl> <int>
    #> 1     1     1
    #> 2     1     2
    #> 3    10    NA
    #> 4    11    NA
    #> 5    12    NA

Instead of falling back to a data frame, we would like to return a
`<my_tibble>` when combined with a data frame or a tibble. Because this
subclass has more metadata than normal data frames (it has a colour), it
is a *supertype* of tibble and data frame, i.e. it is the richer type.
This is similar to how a grouped tibble is a more general type than a
tibble or a data frame. Conceptually, the latter are pinned to a single
constant group.

The coercion methods for data frames operate in two steps:

- They check for compatible subclass attributes. In our case the tibble
  colour has to be the same, or be undefined.

- They call their parent methods, in this case
  [`tib_ptype2()`](https://vctrs.r-lib.org/reference/df_ptype2.md) and
  [`tib_cast()`](https://vctrs.r-lib.org/reference/df_ptype2.md) because
  we have a subclass of tibble. This eventually calls the data frame
  methods
  [`df_ptype2()`](https://vctrs.r-lib.org/reference/df_ptype2.md) and
  [`tib_ptype2()`](https://vctrs.r-lib.org/reference/df_ptype2.md) which
  match the columns and their types.

This process should usually be wrapped in two functions to avoid
repetition. Consider exporting these if you expect your class to be
derived by other subclasses.

We first implement a helper to determine if two data frames have
compatible colours. We use the `df_colour()` accessor which returns
`NULL` when the data frame colour is undefined.

    has_compatible_colours <- function(x, y) {
      x_colour <- df_colour(x) %||% df_colour(y)
      y_colour <- df_colour(y) %||% x_colour
      identical(x_colour, y_colour)
    }

Next we implement the coercion helpers. If the colours are not
compatible, we call
[`stop_incompatible_cast()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md)
or
[`stop_incompatible_type()`](https://vctrs.r-lib.org/reference/vctrs-conditions.md).
These strict coercion semantics are justified because in this class
colour is a *data* attribute. If it were a non essential *detail*
attribute, like the timezone in a datetime, we would just standardise it
to the value of the left-hand side.

In simpler cases (like the data.table example), these methods do not
need to take the arguments suffixed in `_arg`. Here we do need to take
these arguments so we can pass them to the `stop_` functions when we
detect an incompatibility. They also should be passed to the parent
methods.

    #' @export
    my_tib_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
      out <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

      if (!has_compatible_colours(x, to)) {
        stop_incompatible_cast(
          x,
          to,
          x_arg = x_arg,
          to_arg = to_arg,
          details = "Can't combine colours."
        )
      }

      colour <- df_colour(x) %||% df_colour(to)
      new_my_tibble(out, colour = colour)
    }
    #' @export
    my_tib_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
      out <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

      if (!has_compatible_colours(x, y)) {
        stop_incompatible_type(
          x,
          y,
          x_arg = x_arg,
          y_arg = y_arg,
          details = "Can't combine colours."
        )
      }

      colour <- df_colour(x) %||% df_colour(y)
      new_my_tibble(out, colour = colour)
    }

Let’s now implement the coercion methods, starting with the self-self
methods.

    #' @export
    vec_ptype2.my_tibble.my_tibble <- function(x, y, ...) {
      my_tib_ptype2(x, y, ...)
    }
    #' @export
    vec_cast.my_tibble.my_tibble <- function(x, to, ...) {
      my_tib_cast(x, to, ...)
    }

We can now combine compatible instances of our class!

    vec_rbind(red, red)
    #> <my_tibble: red>
    #>       x     y
    #>   <dbl> <int>
    #> 1     1     1
    #> 2     1     2
    #> 3     1     1
    #> 4     1     2

    vec_rbind(green, green)
    #> <my_tibble: green>
    #>   z
    #>   <lgl>
    #> 1 TRUE
    #> 2 TRUE

    vec_rbind(green, red)
    #> Error in `my_tib_ptype2()`:
    #> ! Can't combine `..1` <my_tibble> and `..2` <my_tibble>.
    #> Can't combine colours.

The methods for combining our class with tibbles follow the same
pattern. For ptype2 we return our class in both cases because it is the
richer type:

    #' @export
    vec_ptype2.my_tibble.tbl_df <- function(x, y, ...) {
      my_tib_ptype2(x, y, ...)
    }
    #' @export
    vec_ptype2.tbl_df.my_tibble <- function(x, y, ...) {
      my_tib_ptype2(x, y, ...)
    }

For cast are careful about returning a tibble when casting to a tibble.
Note the call to
[`vctrs::tib_cast()`](https://vctrs.r-lib.org/reference/df_ptype2.md):

    #' @export
    vec_cast.my_tibble.tbl_df <- function(x, to, ...) {
      my_tib_cast(x, to, ...)
    }
    #' @export
    vec_cast.tbl_df.my_tibble <- function(x, to, ...) {
      tib_cast(x, to, ...)
    }

From this point, we get correct combinations with tibbles:

    vec_rbind(red, tibble::tibble(x = 10:12))
    #> <my_tibble: red>
    #>       x     y
    #>   <dbl> <int>
    #> 1     1     1
    #> 2     1     2
    #> 3    10    NA
    #> 4    11    NA
    #> 5    12    NA

However we are not done yet. Because the coercion hierarchy is different
from the class hierarchy, there is no inheritance of coercion methods.
We’re not getting correct behaviour for data frames yet because we
haven’t explicitly specified the methods for this class:

    vec_rbind(red, data.frame(x = 10:12))
    #> # A tibble: 5 x 2
    #>       x     y
    #>   <dbl> <int>
    #> 1     1     1
    #> 2     1     2
    #> 3    10    NA
    #> 4    11    NA
    #> 5    12    NA

Let’s finish up the boiler plate:

    #' @export
    vec_ptype2.my_tibble.data.frame <- function(x, y, ...) {
      my_tib_ptype2(x, y, ...)
    }
    #' @export
    vec_ptype2.data.frame.my_tibble <- function(x, y, ...) {
      my_tib_ptype2(x, y, ...)
    }

    #' @export
    vec_cast.my_tibble.data.frame <- function(x, to, ...) {
      my_tib_cast(x, to, ...)
    }
    #' @export
    vec_cast.data.frame.my_tibble <- function(x, to, ...) {
      df_cast(x, to, ...)
    }

This completes the implementation:

    vec_rbind(red, data.frame(x = 10:12))
    #> <my_tibble: red>
    #>       x     y
    #>   <dbl> <int>
    #> 1     1     1
    #> 2     1     2
    #> 3    10    NA
    #> 4    11    NA
    #> 5    12    NA
