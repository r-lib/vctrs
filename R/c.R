#' Combine many vectors into one vector
#'
#' Combine all arguments into a new vector of common type.
#'
#' @param ... Vectors to coerce. All vectors must be 1d (i.e. no data
#'   frames, matrices or arrays).
#' @return A vector with class given by `.ptype`, and length equal to the
#'   sum of the lengths of the contents of `...`.
#'
#'   The vector will have names if the individual components have names
#'   (inner names) or if the arguments are named (outer names). If both
#'   inner and outer names are present, they are combined with a `.`.
#' @inheritParams vec_ptype
#' @seealso [vec_cbind()]/[vec_rbind()] for combining data frames by rows
#'   or columns.
#' @export
#' @examples
#' vec_c(FALSE, 1L, 1.5)
#' vec_c(FALSE, 1L, "x", .ptype = character())
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
vec_c <- function(..., .ptype = NULL) {
  args <- list2(...)

  dims <- map_int(args, vec_dims)
  if (any(dims > 1)) {
    stop("Inputs must be 1d", call. = FALSE)
  }

  ptype <- vec_ptype(!!!args, .ptype = .ptype)[[1]]
  if (is_unknown(ptype))
    return(NULL)

  ns <- map_int(args, length)
  out <- vec_rep(ptype, sum(ns))
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

    x <- vec_cast(args[[i]], to = ptype)

    names[pos:(pos + n - 1)] <- outer_names(names(args)[[i]], names(args[[i]]), length(x))
    out[pos:(pos + n - 1)] <- x
    pos <- pos + n
  }

  names(out) <- names

  out
}
