#' Cast a list to vector of specific type
#'
#' This is a function for developers to use when extending vctrs. It casts
#' a list to a more specific vectoring type, keeping the length constant.
#' It does this by discarding (with a warning), any elements after the 1.
#' It is called from `vec_cast.XYZ.list()` methods to preserve symmetry with
#' `vec_cast.list.XYZ()`.
#'
#' See `vignette("s3-vector")` for details.
#'
#' @param x A list
#' @param to Type to coerce to
#' @export
#' @keywords internal
vec_list_cast <- function(x, to) {
  ns <- map_int(x, vec_size)

  n <- vec_size(x)
  out <- vec_na(to, n)

  for (i in seq_len(n)) {
    val <- x[[i]]
    if (length(val) == 0)
      next

    vec_slice(out, i) <- vec_cast(vec_slice(val, 1L), to)
  }

  if (!is.object(to)) {
    out <- shape_broadcast(out, to)
  }

  maybe_lossy_cast(
    out, x, to,
    lossy = !ns %in% c(0L, 1L),
    .subclass = "vctrs_error_cast_lossy_list"
  )
}
