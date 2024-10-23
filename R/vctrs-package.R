#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Defines new notions of prototype and size that are
#' used to provide tools for consistent and well-founded type-coercion
#' and size-recycling, and are in turn connected to ideas of type- and
#' size-stability useful for analysing function interfaces.
#'
#' @keywords internal
#' @import rlang
#' @useDynLib vctrs, .registration = TRUE
"_PACKAGE"

release_extra_revdeps <- function() {
  # Extra revdeps to run before release.
  # Recognized by `usethis::use_release_issue()`.
  c("dplyr", "tidyr", "purrr")
}
