# Example usage of ptype2 and cast restart. This handler treats any
# input that inherits from <ordered> as a <factor>. In other words, it
# allows incompatible <ordered> inputs to benefit from all <factor>
# coercion methods.
with_ordered_restart <- function(expr) {
  withCallingHandlers(
    expr,
    vctrs_error_incompatible_type = function(cnd) {
      x <- cnd[["x"]]
      y <- cnd[["y"]]

      restart <- FALSE

      if (is.ordered(x)) {
        restart <- TRUE
        x <- factor(as.character(x), levels = levels(x))
      }
      if (is.ordered(y)) {
        restart <- TRUE
        y <- factor(as.character(y), levels = levels(y))
      }

      # Don't recurse and let ptype2 error keep its course
      if (!restart) {
        return(zap())
      }

      x_arg <- cnd[["x_arg"]]
      y_arg <- cnd[["y_arg"]]
      call <- cnd[["call"]]

      # Recurse with character methods and restart with the result
      switch(
        cnd[["type"]],
        ptype2 = {
          out <- vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
          restart <- "vctrs_restart_ptype2"
        },
        cast = {
          out <- vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
          restart <- "vctrs_restart_cast"
        },
        abort("Unexpected incompatible-type field.", .internal = TRUE)
      )

      # Old-R compat for `tryInvokeRestart()`
      try_restart <- function(restart, ...) {
        if (!is_null(findRestart(restart))) {
          invokeRestart(restart, ...)
        }
      }
      try_restart(restart, out)
    }
  )
}
