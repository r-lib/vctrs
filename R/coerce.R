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
#' \figure{types.png}
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
#'
vec_coerce <- function(..., .strict = TRUE) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  type <- max.vec_type(!!!args, strict = .strict)

  # Should return ListOf<type>
  map(args, function(val) vectype_coerce(type$prototype, val))
}

#' Concatenate vectors
#'
#' Concatenate vectors using the vctr coercion rules as explained in
#' [vec_coerce()].
#'
#' @inheritParams vec_coerce
#' @export
#' @examples
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
vec_c <- function(..., .strict = TRUE) {
  args <- list2(...)

  type <- reduce(args, vectype_max, strict = .strict)
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
