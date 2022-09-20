#' Proxy and restore
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `vec_proxy()` returns the data structure containing the values of a
#' vector. This data structure is usually the vector itself. In this
#' case the proxy is the [identity function][base::identity], which is
#' the default `vec_proxy()` method.
#'
#' Only experts should implement special `vec_proxy()` methods, for
#' these cases:
#'
#' - A vector has vectorised attributes, i.e. metadata for
#'   each element of the vector. These _record types_ are implemented
#'   in vctrs by returning a data frame in the proxy method. If you're
#'   starting your class from scratch, consider deriving from the
#'   [`rcrd`][new_rcrd] class. It implements the appropriate data
#'   frame proxy and is generally the preferred way to create a record
#'   class.
#'
#' - When you're implementing a vector on top of a non-vector type,
#'   like an environment or an S4 object. This is currently only
#'   partially supported.
#'
#' - S3 lists are considered scalars by default. This is the safe
#'   choice for list objects such as returned by `stats::lm()`. To
#'   declare that your S3 list class is a vector, you normally add
#'   `"list"` to the right of your class vector. Explicit inheritance
#'   from list is generally the preferred way to declare an S3 list in
#'   R, for instance it makes it possible to dispatch on
#'   `generic.list` S3 methods.
#'
#'   If you can't modify your class vector, you can implement an
#'   identity proxy (i.e. a proxy method that just returns its input)
#'   to let vctrs know this is a vector list and not a scalar.
#'
#' `vec_restore()` is the inverse operation of `vec_proxy()`. It
#' should only be called on vector proxies.
#'
#' - It undoes the transformations of `vec_proxy()`.
#'
#' - It restores attributes and classes. These may be lost when the
#'   memory values are manipulated. For example slicing a subset of a
#'   vector's proxy causes a new proxy to be allocated.
#'
#' By default vctrs restores all attributes and classes
#' automatically. You only need to implement a `vec_restore()` method
#' if your class has attributes that depend on the data.
#'
#' @param x A vector.
#' @inheritParams rlang::args_dots_empty
#'
#' @section Proxying:
#'
#' You should only implement `vec_proxy()` when your type is designed
#' around a non-vector class. I.e. anything that is not either:
#'
#' * An atomic vector
#' * A bare list
#' * A data frame
#'
#' In this case, implement `vec_proxy()` to return such a vector
#' class. The vctrs operations such as [vec_slice()] are applied on
#' the proxy and `vec_restore()` is called to restore the original
#' representation of your type.
#'
#' The most common case where you need to implement `vec_proxy()` is
#' for S3 lists. In vctrs, S3 lists are treated as scalars by
#' default. This way we don't treat objects like model fits as
#' vectors. To prevent vctrs from treating your S3 list as a scalar,
#' unclass it in the `vec_proxy()` method. For instance, here is the
#' definition for `list_of`:
#'
#' ```
#' vec_proxy.vctrs_list_of <- function(x) {
#'   unclass(x)
#' }
#' ```
#'
#' Another case where you need to implement a proxy is [record
#' types][new_rcrd]. Record types should return a data frame, as in
#' the `POSIXlt` method:
#'
#' ```
#' vec_proxy.POSIXlt <- function(x) {
#'   new_data_frame(unclass(x))
#' }
#' ```
#'
#' Note that you don't need to implement `vec_proxy()` when your class
#' inherits from `vctrs_vctr` or `vctrs_rcrd`.
#'
#'
#' @section Restoring:
#'
#' A restore is a specialised type of cast, primarily used in
#' conjunction with `NextMethod()` or a C-level function that works on
#' the underlying data structure. A `vec_restore()` method can make
#' the following assumptions about `x`:
#'
#' * It has the correct type.
#' * It has the correct names.
#' * It has the correct `dim` and `dimnames` attributes.
#' * It is unclassed. This way you can call vctrs generics with `x`
#'   without triggering an infinite loop of restoration.
#'
#' The length may be different (for example after [vec_slice()] has
#' been called), and all other attributes may have been lost. The
#' method should restore all attributes so that after restoration,
#' `vec_restore(vec_data(x), x)` yields `x`.
#'
#' To understand the difference between `vec_cast()` and `vec_restore()`
#' think about factors: it doesn't make sense to cast an integer to a factor,
#' but if `NextMethod()` or another low-level function has stripped attributes,
#' you still need to be able to restore them.
#'
#' The default method copies across all attributes so you only need to
#' provide your own method if your attributes require special care
#' (i.e. they are dependent on the data in some way). When implementing
#' your own method, bear in mind that many R users add attributes to track
#' additional metadata that is important to them, so you should preserve any
#' attributes that don't require special handling for your class.
#'
#' @section Dependencies:
#' - `x` must be a vector in the vctrs sense (see [vec_is()])
#' - By default the underlying data is returned as is (identity proxy)
#'
#' All vector classes have a proxy, even those who don't implement any
#' vctrs methods. The exception is S3 lists that don't inherit from
#' `"list"` explicitly. These might have to implement an identity
#' proxy for compatibility with vctrs (see discussion above).
#'
#' @keywords internal
#' @export
vec_proxy <- function(x, ...) {
  check_dots_empty0(...)
  return(.Call(ffi_vec_proxy, x))
  UseMethod("vec_proxy")
}
#' @export
vec_proxy.default <- function(x, ...) {
  x
}

#' @rdname vec_proxy
#' @param to The original vector to restore to.
#' @export
vec_restore <- function(x, to, ...) {
  check_dots_empty0(...)
  return(.Call(ffi_vec_restore, x, to))
  UseMethod("vec_restore", to)
}
vec_restore_dispatch <- function(x, to, ...) {
  UseMethod("vec_restore", to)
}
#' @export
vec_restore.default <- function(x, to, ...) {
  .Call(ffi_vec_restore_default, x, to)
}
vec_restore_default <- function(x, to, ...) {
  .Call(ffi_vec_restore_default, x, to)
}

vec_proxy_recurse <- function(x, ...) {
  .Call(ffi_vec_proxy_recurse, x)
}
vec_restore_recurse <- function(x, to, ...) {
  .Call(ffi_vec_restore_recurse, x, to)
}

#' Extract underlying data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' Extract the data underlying an S3 vector object, i.e. the underlying
#' (named) atomic vector, data frame, or list.
#'
#' @param x A vector or object implementing `vec_proxy()`.
#' @return The data underlying `x`, free from any attributes except the names.
#'
#' @section Difference with `vec_proxy()`:
#'
#' * `vec_data()` returns unstructured data. The only attributes
#'   preserved are names, dims, and dimnames.
#'
#'   Currently, due to the underlying memory architecture of R, this
#'   creates a full copy of the data for atomic vectors.
#'
#' * `vec_proxy()` may return structured data. This generic is the
#'   main customisation point for accessing memory values in vctrs,
#'   along with [vec_restore()].
#'
#'   Methods must return a vector type. Records and data frames will
#'   be processed rowwise.
#'
#' @keywords internal
#' @export
vec_data <- function(x) {
  vec_assert(x)
  x <- vec_proxy(x)

  if (is.data.frame(x)) {
    return(new_data_frame(x, row.names = .row_names_info(x, 0L)))
  }

  if (has_dim(x)) {
    x <- vec_set_attributes(x, list(dim = dim(x), dimnames = dimnames(x)))
  } else {
    x <- vec_set_attributes(x, list(names = names(x)))
  }

  # Reset S4 bit in vector-like S4 objects
  unset_s4(x)
}
unset_s4 <- function(x) {
  .Call(ffi_unset_s4, x)
}
