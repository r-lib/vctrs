#' Extract underlying data
#'
#' @description
#'
#' Extract the data underlying an S3 vector object, i.e. the underlying
#' (named) atomic vector or list.
#'
#' * `vec_data()` returns unstructured data. The only attributes
#'   preserved are names.
#'
#'   Currently, due to the underlying memory architecture of R, this
#'   creates a full copy of the data.
#'
#' * `vec_proxy()` may return structured data. This generic is the
#'   main customisation point in vctrs, along with [vec_restore()].
#'   You should only implement it when your type is not its own data,
#'   i.e. it's not a vector, data frame, or record type.
#'
#'   Methods must return a vector type. Records and data frames will
#'   be processed rowwise.
#'
#'
#' @param x A vector or object implementing `vec_proxy()`.
#' @export
#' @return The data underlying `x`, free from any attributes except the names.
#' @seealso See [vec_restore()] for the inverse operation: it restores
#'   attributes given a bare vector and a prototype;
#'   `vec_restore(vec_data(x), x)` will always yield `x`.
vec_data <- function(x) {
  if (!is_vector(x)) {
    return(x)
  }

  # TODO: implement with ALTREP to avoid making a copy
  if (is_record(x)) {
    attributes(x) <- list(names = fields(x))
  } else {
    attributes(x) <- list(names = names(x))
  }

  x
}
#' @rdname vec_data
#' @export
vec_proxy <- function(x) {
  if (vec_is_data_vector(x) || is_null(x)) {
    return(x)
  } else {
    return(vec_proxy_dispatch(x))
  }
  UseMethod("vec_proxy")
}
vec_proxy_dispatch <- function(x) {
  UseMethod("vec_proxy")
}
#' @export
vec_proxy.default <- function(x) {
  vec_assert(x)
  stop_unimplemented(x, "vec_proxy")
}

vec_is_data_vector <- function(x) {
  is_atomic(x) || is_bare_list(x) || is.data.frame(x)
}

is_record <- function(x) {
  UseMethod("is_record")
}
is_record.POSIXlt <- function(x) TRUE
is_record.vctrs_rcrd <- function(x) TRUE
is_record.default <- function(x) FALSE

