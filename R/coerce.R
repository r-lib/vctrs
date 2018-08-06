#' Coerce vectors to shared type
#'
#' This function is a useful as a building block for other functions. The
#' implementation also illustrates the key ideas of the vctrs type system.
#'
#' @param ... Vectors to coerce.
#' @param .type If `NULL`, the default, the output type is determined by
#'   computing the common type across all inputs.
#'
#'   Alternatively, you can supply `.type` to force the output to have known
#'   type, or to die trying. `.type = character()` and `.type = list()` will
#'   succeed for all vectors. See [vec_cast()] for more details.
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

find_type <- function(x, .type = NULL) {
  if (!is.null(.type)) {
    .type
  } else if (isTRUE(getOption("vctrs.no_guessing"))) {
    stop("strict mode is activated; you must supply .type", call. = FALSE)
  } else {
    reduce(x, vec_type2, .init = NULL)
  }
}
