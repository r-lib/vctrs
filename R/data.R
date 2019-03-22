#' Extract underlying data
#'
#' Extract the data underlying an S3 vector object, i.e. the underlying
#' (named) atomic vector or list.
#' Currently, due to the underlying memory architecture
#' of R, this creates a full copy of the data.
#'
#' @param x A vector
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

#' Extract vector data
#'
#' Unlike [vec_data()], this doesn't remove attributes.
#'
#' @param x A vector.
#'
#' @export
vec_proxy <- function(x) {
  if (vec_is_data_vector(x) || is_null(x)) {
    return(x)
  }

  out <- vec_proxy_dispatch(x)
  if (vec_is_data_vector(out)) {
    return(out)
  }

  abort("Internal error: `vec_proxy()` must return a data vector.")
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
  FALSE ||
    is_atomic(x) ||
    is_bare_list(x) ||
    is_record(x) ||
    is.data.frame(x)
}

is_record <- function(x) {
  UseMethod("is_record")
}
is_record.POSIXlt <- function(x) TRUE
is_record.vctrs_rcrd <- function(x) TRUE
is_record.default <- function(x) FALSE

