#' Equality proxy
#'
#' Returns a proxy object (i.e. an atomic vector or data frame of atomic
#' vectors). For [vctr]s, this determines the behaviour of `==` and
#' `!=` (via [vec_equal()]); [unique()], [duplicated()] (via
#' [vec_unique()] and [vec_duplicate_detect()]).
#'
#' @param x A vector x.
#' @return A 1d atomic vector or a data frame.
#' @keywords internal
#' @export
vec_proxy_equality <- function(x) {
  UseMethod("vec_proxy_equality")
}

#' @export
vec_proxy_equality.data.frame <- function(x) {
  is_list <- map_lgl(x, is.list)
  x[is_list] <- lapply(x[is_list], vec_proxy_equality)
  x
}

#' @export
vec_proxy_equality.POSIXlt <- function(x) {
  new_data_frame(vec_data(x), length(x))
}

#' @export
vec_proxy_equality.default <- function(x) {
  if (is.list(x)) {
    lapply(x, vec_data)
  } else {
    vec_data(x)
  }
}

#' Test if two vectors are equal
#'
#' @inheritParams vec_compare
#' @return A logical vector. Will only contain `NA`s if `na_equal` is `FALSE`.
#' @export
#' @examples
#' vec_equal(c(TRUE, FALSE, NA), FALSE)
#' vec_equal(c(TRUE, FALSE, NA), FALSE, na_equal = TRUE)
#'
#' vec_equal(5, 1:10)
#' vec_equal("d", letters[1:10])
#'
#' df <- data.frame(x = c(1, 1, 2), y = c(1, 2, 1))
#' vec_equal(df, data.frame(x = 1, y = 2))
vec_equal <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  args <- vec_recycle(x, y)
  args <- vec_coerce(!!!args, .ptype = .ptype)
  .Call(
    vctrs_equal,
    vec_proxy_equality(args[[1]]),
    vec_proxy_equality(args[[2]]),
    na_equal
  )
}

obj_equal <- function(x, y) {
  .Call(vctrs_equal_object, x, y)
}
