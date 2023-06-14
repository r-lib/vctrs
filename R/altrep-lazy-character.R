#' Lazy character vector
#'
#' `new_lazy_character()` takes a function with no arguments which must return
#' a character vector of arbitrary length. The function will be evaluated
#' exactly once whenever any properties of the character vector are required
#' (including the length or any vector elements).
#'
#' A "real" production level implementation might work more like
#' `carrier::crate()`, where the function is isolated and users must explicitly
#' provide any data required to evaluate the function, since the time of
#' evaluation is unknown.
#'
#' As of June 2023, running `x <- new_lazy_character(~ c("x", "y"))` in the
#' RStudio console will call the ALTREP length method, which materializes the
#' object. Doing this in a terminal session running R does not, so it is an
#' RStudio issue. This doesn't affect tests run within a `test_that()` block.
#'
#' @param fn A function with no arguments returning a character vector.
#'
#' @noRd
new_lazy_character <- function(fn) {
  fn <- as_function(fn)
  .Call(ffi_altrep_new_lazy_character, fn)
}

lazy_character_is_materialized <- function(x) {
  .Call(ffi_altrep_lazy_character_is_materialized, x)
}
