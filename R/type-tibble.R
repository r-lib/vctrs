# Coercion ----------------------------------------------------------------

df_as_tibble <- function(df) {
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

# Conditionally registered in .onLoad()
vec_ptype2.tbl_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.tbl_df", y)
}
vec_ptype2.tbl_df.default <- function(x, y, ...) {
  # FIXME: Do we need some sort of `next_vec_type2()`?
  vec_ptype2.data.frame(x, y, ...)
}

vec_ptype2.tbl_df.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  df_as_tibble(.Call(vctrs_type2_df_df, x, y, x_arg, y_arg))
}
vec_ptype2.data.frame.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  df_as_tibble(.Call(vctrs_type2_df_df, x, y, x_arg, y_arg))
}


tbl_ptype2.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(x, c("tbl_df", "tbl", "data.frame"))) {
    UseMethod("tbl_ptype2.tbl_df", y)
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
tbl_ptype2.tbl_df.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}

tbl_ptype2.tbl_df.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(y, "data.frame")) {
    tibble::tibble()
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
tbl_ptype2.data.frame.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (inherits_only(y, c("tbl_df", "tbl", "data.frame"))) {
    tibble::tibble()
  } else {
    stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}

#' Common prototype for grouped data frames
#' @inheritParams vec_ptype2
#' @export vec_ptype2.grouped_df
#' @export
#' @method vec_ptype2 grouped_df
vec_ptype2.grouped_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.grouped_df")
}
#' @export
#' @method vec_ptype2.grouped_df default
vec_ptype2.grouped_df.default <- function(x, y, ...) {
  UseMethod("vec_ptype2.grouped_df")
}

#' @export
#' @method vec_ptype2.grouped_df grouped_df
vec_ptype2.grouped_df.grouped_df <- function(x, y, ...) {
  ptype <- vec_ptype2(as.data.frame(x), as.data.frame(y))
  groups <- union(dplyr::group_vars(x), dplyr::group_vars(y))
  dplyr::grouped_df(ptype, vars = groups)
}

#' @export
#' @method vec_ptype2.data.frame grouped_df
vec_ptype2.data.frame.grouped_df <- function(x, y, ...) {
  ptype <- vec_ptype2(x, as.data.frame(y))
  dplyr::grouped_df(ptype, vars = dplyr::group_vars(y))
}
#' @export
#' @method vec_ptype2.grouped_df data.frame
vec_ptype2.grouped_df.data.frame <- function(x, y, ...) {
  ptype <- vec_ptype2(as.data.frame(x), y)
  dplyr::grouped_df(ptype, vars = dplyr::group_vars(x))
}

#' @export
vec_proxy.grouped_df <- function(x, ...) {
  x <- grouped_df_wrap(x)
  NextMethod()
}
#' @export
vec_restore.grouped_df <- function(x, to, ...) {
  grouped_df_unwrap(NextMethod())
}

grouped_df_wrap <- function(x) {
  groups <- dplyr::group_vars(x)
  x[groups] <- map2(groups, x[groups], wrap_group_col)
  x
}
wrap_group_col <- function(name, x) {
  new_data_frame(
    list2(!!name := x),
    n = length(x),
    class = "rlib__grouped_column"
  )
}

grouped_df_unwrap <- function(x) {
  groups_ind <- map_lgl(x, is_wrapped_group_col)
  groups_vars <- names(x)[groups_ind]

  x <- as.data.frame(x)
  x[] <- map_if(x, groups_ind, `[[`, 1L)

  # If recomputing groups every time is too slow, perhaps we can cache
  # the groups
  dplyr::grouped_df(x, groups_vars)
}
is_wrapped_group_col <- function(x) {
  inherits(x, "rlib__grouped_column")
}
