# Vector checks

- `obj_is_vector()` tests if `x` is considered a vector in the vctrs
  sense. See *Vectors and scalars* below for the exact details.

- `obj_check_vector()` uses `obj_is_vector()` and throws a standardized
  and informative error if it returns `FALSE`.

- `vec_check_size()` tests if `x` has size `size`, and throws an
  informative error if it doesn't.

- `vec_check_recyclable()` tests if `x` can recycle to size `size`, and
  throws an informative error if it can't.

## Usage

``` r
obj_is_vector(x)

obj_check_vector(x, ..., arg = caller_arg(x), call = caller_env())

vec_check_size(x, size, ..., arg = caller_arg(x), call = caller_env())

vec_check_recyclable(x, size, ..., arg = caller_arg(x), call = caller_env())
```

## Arguments

- x:

  For `obj_*()` functions, an object. For `vec_*()` functions, a vector.

- ...:

  These dots are for future extensions and must be empty.

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- size:

  The size to check for compatibility with.

## Value

- `obj_is_vector()` returns a single `TRUE` or `FALSE`.

- `obj_check_vector()` returns `NULL` invisibly, or errors.

- `vec_check_size()` returns `NULL` invisibly, or errors.

- `vec_check_recyclable()` returns `NULL` invisibly, or errors.

## Vectors and scalars

Informally, a vector is a collection that makes sense to use as column
in a data frame. The following rules define whether or not `x` is
considered a vector.

If no [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
method has been registered, `x` is a vector if:

- The [base type](https://rdrr.io/r/base/typeof.html) of the object is
  atomic: `"logical"`, `"integer"`, `"double"`, `"complex"`,
  `"character"`, or `"raw"`.

- `x` is a list, as defined by
  [`obj_is_list()`](https://vctrs.r-lib.org/reference/obj_is_list.md).

- `x` is a [data.frame](https://rdrr.io/r/base/data.frame.html).

If a [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
method has been registered, `x` is a vector if:

- The proxy satisfies one of the above conditions.

- The base type of the proxy is `"list"`, regardless of its class. S3
  lists are thus treated as scalars unless they implement a
  [`vec_proxy()`](https://vctrs.r-lib.org/reference/vec_proxy.md)
  method.

Otherwise an object is treated as scalar and cannot be used as a vector.
In particular:

- `NULL` is not a vector.

- S3 lists like `lm` objects are treated as scalars by default.

- Objects of type [expression](https://rdrr.io/r/base/expression.html)
  are not treated as vectors.

## Technical limitations

- Support for S4 vectors is currently limited to objects that inherit
  from an atomic type.

- Subclasses of [data.frame](https://rdrr.io/r/base/data.frame.html)
  that *append* their class to the back of the `"class"` attribute are
  not treated as vectors. If you inherit from an S3 class, always
  prepend your class to the front of the `"class"` attribute for correct
  dispatch. This matches our general principle of allowing subclasses
  but not mixins.

## Examples

``` r
obj_is_vector(1)
#> [1] TRUE

# Data frames are vectors
obj_is_vector(data_frame())
#> [1] TRUE

# Bare lists are vectors
obj_is_vector(list())
#> [1] TRUE

# S3 lists are vectors if they explicitly inherit from `"list"`
x <- structure(list(), class = c("my_list", "list"))
obj_is_list(x)
#> [1] TRUE
obj_is_vector(x)
#> [1] TRUE

# But if they don't explicitly inherit from `"list"`, they aren't
# automatically considered to be vectors. Instead, vctrs considers this
# to be a scalar object, like a linear model returned from `lm()`.
y <- structure(list(), class = "my_list")
obj_is_list(y)
#> [1] FALSE
obj_is_vector(y)
#> [1] FALSE

# `obj_check_vector()` throws an informative error if the input
# isn't a vector
try(obj_check_vector(y))
#> Error in eval(expr, envir) : 
#>   `y` must be a vector, not a <my_list> object.
#> ✖ Detected incompatible scalar S3 list. To be treated as a vector, the object must explicitly inherit from <list> or should implement a `vec_proxy()` method. Class: <my_list>.
#> ℹ If this object comes from a package, please report this error to the package author.
#> ℹ Read our FAQ about creating vector types (`?vctrs::howto_faq_fix_scalar_type_error`) to learn more.

# `vec_check_size()` throws an informative error if the size of the
# input doesn't match `size`
vec_check_size(1:5, size = 5)
try(vec_check_size(1:5, size = 4))
#> Error in eval(expr, envir) : `1:5` must have size 4, not size 5.

# `vec_check_recyclable()` throws an informative error if the input can't
# recycle to size `size`
vec_check_recyclable(1:5, size = 5)
vec_check_recyclable(1, size = 5)
try(vec_check_recyclable(1:2, size = 5))
#> Error in eval(expr, envir) : Can't recycle `1:2` (size 2) to size 5.
```
