# Constructor and basic methods  ---------------------------------------------

#' rcrd (record) S3 class
#'
#' The rcrd class extends [vctr]. A rcrd is composed of 1 or more [field]s,
#' which must be vectors of the same length. Is designed specifically for
#' classes that can naturally be decomposed into multiple vectors of the same
#' length, like [POSIXlt], but where the organisation should be considered
#' an implementation detail invisible to the user (unlike a [data.frame]).
#'
#' @param fields A list or a data frame. Lists must be rectangular
#'   (same sizes), and contain uniquely named vectors (at least
#'   one). `fields` is validated with [df_list()] which recycles
#'   columns to the same size.
#' @param ... Additional attributes
#' @param class Name of subclass.
#' @export
#' @aliases ses rcrd
#' @keywords internal
new_rcrd <- function(fields, ..., class = character()) {
  fields <- df_list(!!!fields)
  if (!length(fields)) {
    abort("`fields` must be a list of length 1 or greater.")
  }
  structure(fields, ..., class = c(class, "vctrs_rcrd", "vctrs_vctr"))
}

#' @export
vec_proxy.vctrs_rcrd <- function(x, ...) {
  new_data_frame(unclass(x))
}
#' @export
vec_restore.vctrs_rcrd <- function(x, to, ...) {
  x <- NextMethod()
  attr(x, "row.names") <- NULL
  x
}

#' @export
length.vctrs_rcrd <- function(x) {
  vec_size(x)
}

#' @export
names.vctrs_rcrd <- function(x) {
  NULL
}

#' @export
format.vctrs_rcrd <- function(x, ...) {
  if (inherits(x, "vctrs_foobar")) {
    # For unit tests
    exec("paste", !!!vec_data(x), sep = ":")
  } else {
    stop_unimplemented(x, "format")
  }
}

#' @export
obj_str_data.vctrs_rcrd <- function(x, ...) {
  obj_str_leaf(x, ...)
}

#' @method vec_cast vctrs_rcrd
#' @export
vec_cast.vctrs_rcrd <- function(x, to, ...) UseMethod("vec_cast.vctrs_rcrd")

#' @method vec_cast.vctrs_rcrd vctrs_rcrd
#' @export
vec_cast.vctrs_rcrd.vctrs_rcrd <- function(x, to, ..., x_arg = x_arg, to_arg = to_arg) {
  # This assumes that we don't have duplicate field names,
  # which is verified even in the constructor.
  if (!setequal(fields(x), fields(to))) {
    stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }

  new_data <- map2(
    vec_data(x)[fields(to)],
    vec_data(to),
    vec_cast
  )

  new_rcrd(new_data)
}


# Subsetting --------------------------------------------------------------

#' @export
`[.vctrs_rcrd` <-  function(x, i, ...) {
  vec_index(x, i, ...)
}

#' @export
`[[.vctrs_rcrd` <- function(x, i, ...) {
  out <- vec_slice(vec_data(x), i)
  vec_restore(out, x)
}

#' @export
`$.vctrs_rcrd` <- function(x, i, ...) {
  stop_unsupported(x, "subsetting with $")
}

#' @export
rep.vctrs_rcrd <- function(x, ...) {
  out <- lapply(vec_data(x), base_vec_rep, ...)
  vec_restore(out, x)
}

#' @export
`length<-.vctrs_rcrd` <- function(x, value) {
  out <- lapply(vec_data(x), `length<-`, value)
  vec_restore(out, x)
}

# Replacement -------------------------------------------------------------

#' @export
`[[<-.vctrs_rcrd` <- function(x, i, value) {
  force(i)
  x[i] <- value
  x
}

#' @export
`$<-.vctrs_rcrd` <- function(x, i, value) {
  stop_unsupported(x, "subset assignment with $")
}

#' @export
`[<-.vctrs_rcrd` <- function(x, i, value) {
  i <- maybe_missing(i, TRUE)
  value <- vec_cast(value, x)
  out <- vec_assign(vec_data(x), i, vec_data(value))
  vec_restore(out, x)
}

# Equality and ordering ---------------------------------------------------

# FIXME

#' @export
vec_proxy_compare.vctrs_rcrd <- function(x, ...) {
  new_data_frame(vec_data(x), n = length(x))
}

#' @export
vec_math.vctrs_rcrd <- function(.fn, .x, ...) {
  stop_unsupported(.x, "vec_math")
}
