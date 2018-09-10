#' Partially specify columns of a data frame
#'
#' This special class can be passed to `.ptype` in order to specify the
#' types of only some of the columns in a data frame.
#'
#' @param ... Attributes of subclass
#' @export
#' @examples
#' pf <- partial_frame(x = double())
#' pf
#'
#' vec_rbind(
#'   data.frame(x = 1L, y = "a"),
#'   data.frame(x = FALSE, z = 10),
#'   .ptype = partial_frame(x = double(), a = character())
#' )
partial_frame <- function(...) {
  args <- list2(...)
  args <- lapply(args, as_vec_ptype)

  partial <- new_data_frame(args, n = 0)
  new_partial_frame(partial)
}

new_partial_frame <- function(partial = data.frame(), learned = data.frame()) {
  stopifnot(is.data.frame(partial))
  stopifnot(is.data.frame(learned))

  new_partial(
    partial = partial,
    learned = learned,
    class = "vctrs_partial_frame"
  )
}

#' @export
vec_ptype_full.vctrs_partial_frame <- function(x) {
  partial <- attr(x, "partial")
  learned <- attr(x, "learned")
  both <- c(as.list(partial), as.list(learned))

  types <- map_chr(both, vec_ptype_full)
  needs_indent <- grepl("\n", types)
  types[needs_indent] <- map(types[needs_indent], function(x) indent(paste0("\n", x), 4))

  source <- c(rep(" {partial}", length(partial)), rep("", length(learned)))
  names <- paste0("  ", format(names(both)))

  paste0(
    "partial_frame<\n",
    paste0(names, ": ", types, source, collapse = "\n"),
    "\n>"
  )
}

#' @export
vec_ptype_abbr.vctrs_partial_frame <- function(x) {
  "<partial_frame>"
}

vec_type2.vctrs_partial_frame <- function(x, y) {
  UseMethod("vec_type2.vctrs_partial_frame", y)
}

#' @method vec_type2.vctrs_partial_frame data.frame
#' @export
vec_type2.vctrs_partial_frame.data.frame <- function(x, y) {
  partial <- attr(x, "partial")
  learned <- vec_type2(attr(x, "learned"), y)

  new_partial_frame(partial, learned)
}

#' @method vec_type2.data.frame vctrs_partial_frame
#' @export
vec_type2.data.frame.vctrs_partial_frame <- function(x, y) {
  vec_type2.vctrs_partial_frame.data.frame(y, x)
}

#' @export
vec_type_finalise.vctrs_partial_frame <- function(x) {
  partial <- attr(x, "partial")

  out <- attr(x, "learned")
  out[names(partial)] <- partial

  out
}
