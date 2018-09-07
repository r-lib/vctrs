#' Coerce vectors to common type
#'
#' This function is primarily useful as a building block for other functions.
#' The implementation also illustrates the key ideas of the vctrs type system,
#' and allows you to experiment interactively.
#'
#' @param ... Vectors to coerce.
#' @inheritParams vec_ptype
#' @return A [list_of] input vectors coerced to `.ptype`, an error stating
#'   that a common type could not be found, or an error stating that casting
#'   a input to `.ptype` was not possible.
#' @export
#' @examples
#' vec_coerce(factor("a"), factor(c("a", "b")))
#' vec_coerce(factor("a"), Sys.Date(), .ptype = list())
vec_coerce <- function(..., .ptype = NULL) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  ptype <- vec_ptype(!!!args, .ptype = .ptype)[[1]]
  list_of(!!!args, .ptype = ptype)
}
