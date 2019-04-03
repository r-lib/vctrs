#' Combine many vectors into one vector
#'
#' Combine all arguments into a new vector of common type.
#'
#' @section Invariants:
#' * `vec_size(vec_c(x, y)) == vec_size(x) + vec_size(y)`
#' * `vec_type(vec_c(x, y)) == vec_type_common(x, y)`.
#'
#' @param ... Vectors to coerce.
#' @return A vector with class given by `.ptype`, and length equal to the
#'   sum of the `vec_size()` of the contents of `...`.
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

  ptype <- vec_type_common(!!!args, .ptype = .ptype)
  if (is.null(ptype))
    return(NULL)

  ns <- map_int(args, vec_size)
  out <- vec_na(ptype, sum(ns))

  has_names <- !is.null(names(args)) || has_inner_names(args)
  if (has_names) {
    names <- vec_na(character(), sum(ns))
  } else {
    names <- NULL
  }

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    x <- vec_cast(args[[i]], to = ptype)

    if (has_names) {
      n_x <- length(x)
      names_i <- outer_names(names(args)[[i]], vec_names(args[[i]]), n_x)
      names[pos:(pos + n - 1)] <- names_i %||% rep("", times = n_x)
    }

    vec_slice(out, pos:(pos + n - 1)) <- x
    pos <- pos + n
  }

  if (!is.null(names)) {
    if (is_installed("tibble")) {
      names <- tibble::tidy_names(names)
    }
    vec_names(out) <- names
  }

  out
}
