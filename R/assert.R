#' Assert an argument has known prototype and/or size
#'
#' @param x A vector argument to check.
#' @param ptype Prototype to compare against.
#' @param size Size to compare against
#' @return Either an error, or `x`, invisibly.
#' @export
vec_assert <- function(x, ptype = NULL, size = NULL) {
  x_name <- as.character(ensym(x))

  if (!is.null(ptype)) {
    x_type <- vec_type(x)
    ptype <- vec_type(ptype)

    if (!identical(ptype, x_type)) {
      msg <- paste0("`", x_name, "` must be <", vec_ptype_abbr(ptype), ">, not <", vec_ptype_abbr(x_type), ">.")
      abort(msg)
    }
  }

  if (!is.null(size)) {
    size <- vec_recycle(vec_cast(size, integer()), 1L)
    x_size <- vec_size(x)

    if (!identical(size, x_size)) {
      msg <- paste0("`", x_name, "` must have size ", size, ", not size ", x_size, ".")
      abort(msg)
    }
  }

  invisible(x)
}
