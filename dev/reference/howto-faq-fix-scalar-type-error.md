# FAQ - Why isn't my class treated as a vector?

The tidyverse is a bit stricter than base R regarding what kind of
objects are considered as vectors (see the [user
FAQ](https://vctrs.r-lib.org/dev/reference/faq-error-scalar-type.md)
about this topic). Sometimes vctrs won’t treat your class as a vector
when it should.

### Why isn’t my list class considered a vector?

By default, S3 lists are not considered to be vectors by vctrs:

    my_list <- structure(list(), class = "my_class")

    vctrs::vec_is(my_list)
    #> [1] FALSE

To be treated as a vector, the class must either inherit from `"list"`
explicitly:

    my_explicit_list <- structure(list(), class = c("my_class", "list"))
    vctrs::vec_is(my_explicit_list)
    #> [1] TRUE

Or it should implement a
[`vec_proxy()`](https://vctrs.r-lib.org/dev/reference/vec_proxy.md)
method that returns its input if explicit inheritance is not possible or
troublesome:

    #' @export
    vec_proxy.my_class <- function(x, ...) x

    vctrs::vec_is(my_list)
    #> [1] FALSE

Note that explicit inheritance is the preferred way because this makes
it possible for your class to dispatch on `list` methods of S3 generics:

    my_generic <- function(x) UseMethod("my_generic")
    my_generic.list <- function(x) "dispatched!"

    my_generic(my_list)
    #> Error in UseMethod("my_generic"): no applicable method for 'my_generic' applied to an object of class "my_class"

    my_generic(my_explicit_list)
    #> [1] "dispatched!"

### Why isn’t my data frame class considered a vector?

The most likely explanation is that the data frame has not been properly
constructed.

However, if you get an “Input must be a vector” error with a data frame
subclass, it probably means that the data frame has not been properly
constructed. The main cause of these errors are data frames whose *base
class* is not `"data.frame"`:

    my_df <- data.frame(x = 1)
    class(my_df) <- c("data.frame", "my_class")

    vctrs::obj_check_vector(my_df)
    #> Error:
    #> ! `my_df` must be a vector, not a <data.frame> object.
    #> x Detected incompatible data frame structure. A data frame is normally treated as a vector, but an incompatible class ordering was detected. To be compatible, the subclass <my_class> must come before <data.frame>, not after. Class: <data.frame/my_class>.
    #> i If this object comes from a package, please report this error to the package author.
    #> i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

This is problematic as many tidyverse functions won’t work properly:

    dplyr::slice(my_df, 1)
    #> Error in `vec_slice()`:
    #> ! `x` must be a vector, not a <data.frame> object.
    #> x Detected incompatible data frame structure. A data frame is normally treated as a vector, but an incompatible class ordering was detected. To be compatible, the subclass <my_class> must come before <data.frame>, not after. Class: <data.frame/my_class>.
    #> i If this object comes from a package, please report this error to the package author.
    #> i Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

It is generally not appropriate to declare your class to be a superclass
of another class. We generally consider this undefined behaviour (UB).
To fix these errors, you can simply change the construction of your data
frame class so that `"data.frame"` is a base class, i.e. it should come
last in the class vector:

    class(my_df) <- c("my_class", "data.frame")

    vctrs::obj_check_vector(my_df)

    dplyr::slice(my_df, 1)
    #>   x
    #> 1 1
