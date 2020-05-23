
# All methods in this file are conditionally registered in .onLoad()


### `grouped_df` -----------------------------------------------------

group_intersect <- function(x, new) {
  intersect(dplyr::group_vars(x), names(new))
}

vec_restore.grouped_df <- function(x, to, ...) {
  vars <- group_intersect(to, x)
  drop <- dplyr::group_by_drop_default(to)
  dplyr::grouped_df(x, vars, drop = drop)
}


# `vec_ptype2()` -----------------------------------------------------

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
  common <- df_ptype2(x, y, ...)

  x_vars <- dplyr::group_vars(x)
  y_vars <- dplyr::group_vars(y)
  vars <- union(x_vars, y_vars)

  drop <- dplyr::group_by_drop_default(x) && dplyr::group_by_drop_default(y)

  dplyr::grouped_df(common, vars, drop = drop)
}


# `vec_cast()` -------------------------------------------------------

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
  df <- df_cast(x, to, ...)

  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)

  dplyr::grouped_df(df, vars, drop = drop)
}


### `rowwise` --------------------------------------------------------

vec_restore.rowwise_df <- function(x, to, ...) {
  dplyr::rowwise(x)
}


# `vec_ptype2()` -----------------------------------------------------

vec_ptype2.rowwise_df.rowwise_df <- function(x, y, ...) {
  rww_ptype2(x, y, ...)
}

vec_ptype2.rowwise_df.data.frame <- function(x, y, ...) {
  rww_ptype2(x, y, ...)
}
vec_ptype2.data.frame.rowwise_df <- function(x, y, ...) {
  rww_ptype2(x, y, ...)
}

vec_ptype2.rowwise_df.tbl_df <- function(x, y, ...) {
  rww_ptype2(x, y, ...)
}
vec_ptype2.tbl_df.rowwise_df <- function(x, y, ...) {
  rww_ptype2(x, y, ...)
}

rww_ptype2 <- function(x, y, ...) {
  dplyr::rowwise(df_ptype2(x, y, ...))
}


# `vec_cast()` -------------------------------------------------------

vec_cast.rowwise_df.rowwise_df <- function(x, to, ...) {
  rww_cast(x, to, ...)
}

vec_cast.rowwise_df.data.frame <- function(x, to, ...) {
  rww_cast(x, to, ...)
}
vec_cast.data.frame.rowwise_df <- function(x, to, ...) {
  df_cast(x, to, ...)
}

vec_cast.rowwise_df.tbl_df <- function(x, to, ...) {
  rww_cast(x, to, ...)
}
vec_cast.tbl_df.rowwise_df <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

rww_cast <- function(x, to, ...) {
  dplyr::rowwise(df_cast(x, to, ...))
}
