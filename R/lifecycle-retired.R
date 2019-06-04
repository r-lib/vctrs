#' Is a vector empty
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{vctrs:::lifecycle("defunct")}
#'
#' This function is defunct, please use [vec_is_empty()].
#'
#' @param x An object.
#'
#' @keywords internal
#' @export
vec_empty <- function(x) {
  stop_defunct(paste_line(
    "`vec_empty()` is defunct as of vctrs 0.2.0.",
    "Please use `vec_is_empty()` instead."
  ))
}
