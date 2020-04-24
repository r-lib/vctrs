
# `numeric_version` from base ----------------------------------------

#' @export
vec_proxy.numeric_version <- function(x, ...) x


# `data.table` -------------------------------------------------------

delayedAssign("as.data.table", {
  if (is_installed("data.table")) {
    env_get(ns_env("data.table"), "as.data.table")
  }
})

#' @export
vec_ptype2.data.table.data.frame <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.table.tbl_df <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.tbl_df.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}

dt_ptype2 <- function(x, y, ...) {
  as.data.table(df_ptype2(x, y, ...))
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
vec_cast.data.table.tbl_df <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.tbl_df.data.table <- function(x, to, ...) {
  df_cast(x, to, ...)
}

dt_cast <- function(x, to, ...) {
  as.data.table(df_cast(x, to, ...))
}
