
#' Concatenate vectors
#'
#' Concatenate vectors using the vctr coercion rules as explained in
#' [vec_coerce()].
#'
#' @param ... Vectors to coerce. All vectors must be 1d (i.e. no data
#'   frames, matrices or arrays).
#' @return A vector with length equal to the sum of the lengths of the contents
#'   of `...`.
#'
#'   The vector will have names if the individual components have names
#'   (inner names) or if the arguments are named (outer names). If both
#'   inner and outer names are present, they are combined with a `.`.
#' @inheritParams vec_coerce
#' @export
#' @examples
#' vec_c(FALSE, 1L, 1.5)
#' vec_c(FALSE, 1L, "x", .type = character())
#'
#' # Date/times --------------------------
#' c(Sys.Date(), Sys.time())
#' c(Sys.time(), Sys.Date())
#'
#' vec_c(Sys.Date(), Sys.time())
#' vec_c(Sys.time(), Sys.Date())
#'
#' # Factors -----------------------------
#' c(factor("a"), factor("b"))
#' vec_c(factor("a"), factor("b"))
vec_c <- function(..., .type = NULL) {
  args <- list2(...)

  dims <- map_int(args, vec_dims)
  if (any(dims > 1)) {
    stop("Inputs must be 1d", call. = FALSE)
  }

  # Impute least-upper-bound type, if needed
  type <- find_type(args, .type = .type)
  if (is.null(type))
    return(NULL)

  ns <- map_int(args, length)
  out <- vec_rep(type, sum(ns))
  if (is.null(names(args))) {
    names <- NULL
  } else {
    names <- vec_rep(character(), sum(ns))
  }

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    x <- vec_cast(args[[i]], to = type)

    names[pos:(pos + n - 1)] <- outer_names(x, names(args)[[i]])
    out[pos:(pos + n - 1)] <- x
    pos <- pos + n
  }

  names(out) <- names

  out
}
