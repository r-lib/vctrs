#' Coerce vectors to shared type
#'
#' This function is a general tool that will be of most interest when developing
#' new functions, but it also serves as a central place to document design
#' decisions. See [vec_c()] for an application of these principles.
#'
#' @section Coercion rules:
#'
#' vctrs thinks of the vector types as forming a partially ordered set, or
#' poset. Then finding the common type from a set of types is a matter of
#' finding the least-upper-bound; if the least-upper-bound does not exist,
#' there is no common type. This is the case for many pairs of 1d vectors.
#'
#' The poset of the most important base vectors is shown below:
#' (where datetime stands for `POSIXt`, and date for `Date`)
#'
#' \figure{coerce.png}
#'
#' @param ... Vectors to coerce.
#' @param .type Usually, the type of the output is coerced to a type common to
#'   inputs. Alternatively, you can supply `.type` to force the output to
#'   have known type, or to die trying. See [vec_cast()] for more details.
#'   `.type = character()` and `.type = list()` will succeed for all vectors.
#' @return A list of input vectors coerced to shared (least-upper-bound) type,
#'   or an error stating that a common type could not be found.
#' @export
#' @examples
#' vec_coerce(factor("a"), factor(c("a", "b")))
#' vec_coerce(factor("a"), Sys.Date(), .type = list())
vec_coerce <- function(..., .type = NULL) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  type <- find_type(args, .type = .type)

  # Should return ListOf<type>
  as_repeated(map(args, vec_cast, to = type), .type = type)
}

#' Concatenate vectors
#'
#' Concatenate vectors using the vctr coercion rules as explained in
#' [vec_coerce()].
#'
#' @param ... Vectors to coerce. All vectors must be 1d (i.e. no data
#'   frames, matrices or arrays).
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

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    out[pos:(pos + n - 1)] <- vec_cast(args[[i]], to = type)
    pos <- pos + n
  }

  out
}

find_type <- function(x, .type = NULL) {
  if (!is.null(.type)) {
    .type
  } else {
    reduce(x, vectype_max, .init = NULL)
  }
}
