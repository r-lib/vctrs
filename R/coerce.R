#' Coerce vectors to shared type
#'
#' This function is a useful as a building block for other functions. The
#' implementation also illustrates the key ideas of the vctrs type system.
#'
#' @param ... Vectors to coerce.
#' @param .type Usually, the type of the output is coerced to a type common to
#'   inputs. Alternatively, you can supply `.type` to force the output to
#'   have known type, or to die trying. See [vec_cast()] for more details.
#'   `.type = character()` and `.type = list()` will succeed for all vectors.
#' @return A [list_of] input vectors coerced to `.type`, an error stating
#'   that a common type could not be found, or an error stating that casting
#'   a input to `.type` was not possible.
#' @export
#' @examples
#' vec_coerce(factor("a"), factor(c("a", "b")))
#' vec_coerce(factor("a"), Sys.Date(), .type = list())
vec_coerce <- function(..., .type = NULL) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  type <- find_type(args, .type = .type)
  as_list_of(map(args, vec_cast, to = type), .type = type)
}

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

find_type <- function(x, .type = NULL) {
  if (!is.null(.type)) {
    .type
  } else if (isTRUE(getOption("vctrs.no_guessing"))) {
    stop("strict mode is activated; you must supply .type", call. = FALSE)
  } else {
    reduce(x, vec_type2, .init = NULL)
  }
}
