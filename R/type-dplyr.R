
# All methods in this file are conditionally registered in .onLoad()

vec_restore.grouped_df <- function(x, to, ...) {
  check_drop(to)

  vars <- intersect(names(x), dplyr::group_vars(to))
  dplyr::grouped_df(x, vars, drop = TRUE)
}

check_drop <- function(x) {
  if (is_false(dplyr::group_by_drop_default(x))) {
    abort("Grouped data frames created with `drop = FALSE` are unsupported in vctrs.")
  }
}


# `vec_ptype2()` -----------------------------------------------------

vec_ptype2.grouped_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.grouped_df", y)
}

vec_ptype2.grouped_df.grouped_df <- function(x, y, ...) {
  gdf_ptype2(x, y, ...)
}

vec_ptype2.grouped_df.data.frame <- function(x, y, ...) {
  gdf_ptype2(x, y, ...)
}
vec_ptype2.data.frame.grouped_df <- function(x, y, ...) {
  gdf_ptype2(x, y, ...)
}

vec_ptype2.grouped_df.tbl_df <- function(x, y, ...) {
  gdf_ptype2(x, y, ...)
}
vec_ptype2.tbl_df.grouped_df <- function(x, y, ...) {
  gdf_ptype2(x, y, ...)
}

gdf_ptype2 <- function(x, y, ...) {
  check_drop(x)
  check_drop(y)

  common <- df_ptype2(x, y, ...)

  x_vars <- dplyr::group_vars(x)
  y_vars <- dplyr::group_vars(y)
  vars <- union(x_vars, y_vars)

  dplyr::grouped_df(common, vars, drop = TRUE)
}


# `vec_cast()` -------------------------------------------------------

vec_cast.grouped_df <- function(x, to, ...) {
  UseMethod("vec_cast.grouped_df")
}

vec_cast.grouped_df.grouped_df <- function(x, to, ...) {
  gdf_cast(x, to, ...)
}

vec_cast.grouped_df.data.frame <- function(x, to, ...) {
  gdf_cast(x, to, ...)
}
vec_cast.data.frame.grouped_df <- function(x, to, ...) {
  df_cast(x, to, ...)
}

vec_cast.grouped_df.tbl_df <- function(x, to, ...) {
  gdf_cast(x, to, ...)
}
vec_cast.tbl_df.grouped_df <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

gdf_cast <- function(x, to, ...) {
  check_drop(to)

  df <- df_cast(x, to, ...)
  vars <- dplyr::group_vars(to)
  dplyr::grouped_df(df, vars, drop = TRUE)
}
