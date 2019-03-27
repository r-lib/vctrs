#' @keywords internal
#' @import rlang
#' @useDynLib vctrs, .registration = TRUE
"_PACKAGE"


# FIXME: Fix for backtraces in vignettes. Remove once rlang 0.3.3 is out.
#' @export
as.character.rlang_error <- function(x, ...) {
  # Don't generate backtrace or reminder in programmatic uses. Fixes
  # backtraces in knitr.
  x$message
}
