delayedAssign("as.data.table", {
  if (is_installed("data.table")) {
    env_get(ns_env("data.table"), "as.data.table")
  } else {
    function(...) abort("`data.table` must be installed.")
  }
})

dt_ptype2 <- function(x, y, ...) {
  as.data.table(df_ptype2(x, y, ...))
}
dt_cast <- function(x, to, ...) {
  as.data.table(df_cast(x, to, ...))
}

#' @export
vec_ptype2.data.table.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.table.data.frame <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}

#' @export
vec_cast.data.table.data.table <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.data.table.data.frame <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.data.table <- function(x, to, ...) {
  df_cast(x, to, ...)
}

#' @export
vec_ptype_abbr.data.table <- function(x, ...) {
  "dt"
}
