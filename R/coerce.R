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
#' there is no common type.
#'
#' The poset of the most important base vectors is shown below:
#' (where datetime stands for `POSIXt`, and date for `Date`)
#'
#' \figure{coerce.png}
#'
#' Red lines indicate coercions that only occur when `.strict = FALSE`
#' Note that factors are a parametric type; it doesn't make sense to compare
#' factors with different level sets. The rules are slightly more generous
#' than shown in this diagram: if one level set is completely contained
#' within the other, we use the larger set.
#'
#' @param ... Vectors to coerce.
#' @param .strict If `.strict = FALSE`, there will always be a common type for
#'   any pair of vectors. This will be a character vector for factors with
#'   different level sets, and a list for everything else.
#' @return A list of input vectors coerced to shared (least-upper-bound) type,
#'   or an error stating that a common type could not be found.
#' @export
#' @examples
#'
#' vec_coerce(factor("a"), factor(c("a", "b")))
#' vec_coerce(factor("a"), factor("b"), .strict = FALSE)
#' vec_coerce(factor("a"), factor("b"), .type = character())
vec_coerce <- function(..., .strict = TRUE, .type = NULL) {
  args <- list2(...)
  type <- find_type(args, .strict = .strict, .type = .type)

  # Should return ListOf<type>
  map(args, vec_cast, to = type)
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
#'
#' # vctrs considers factors with different level sets to be
#' # different types with no common type, unless you relax the
#' # usual rules, in which case you'll get a character vector
#' vec_c(factor("a"), factor("b"), .strict = FALSE)
#'
#' # You can also supply a type to override the defaults
#' vec_c(factor("a"), factor("b"), .type = character())
#' vec_c(factor("a"), factor("b"), .type = factor(levels = c("a", "b")))
vec_c <- function(..., .strict = TRUE, .type = NULL) {
  args <- list2(...)

  dims <- map_int(args, vec_dims)
  if (any(dims > 1)) {
    stop("Inputs must be 1d", call. = FALSE)
  }

  # Impute least-upper-bound type, if needed
  type <- find_type(args, .strict = .strict, .type = .type)
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

find_type <- function(x, .strict = TRUE, .type = NULL) {
  if (!is.null(.type)) {
    return(.type)
  }

  reduce(x, vectype_max, strict = .strict, .init = NULL)
}
