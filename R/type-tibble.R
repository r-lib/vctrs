# Coercion ----------------------------------------------------------------

df_as_tibble <- function(df) {
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}
new_tibble <- function(x, ...) {
  new_data_frame(x, ..., class = c("tbl_df", "tbl"))
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

# grouped_df ---------------------------------------------------------

#' Statically grouped data frames
#'
#' @section Proxying and restoring:
#'
#' The group locations are transformed to a full size vector of group
#' identifiers and stored in the proxy as a column called
#' `"dplyr:::grouped_df_id"`. Since they are in the proxy, they get
#' sliced along the data. This currently requires expanding the list
#' of locations into the full integer vector, which is slower. The
#' goal is to eventually enable optimisations, especially with sorted
#' data frames.
#'
#' The restore method turns the `id` column back to a list of
#' locations. It needs to check for multiple .id columns, which could
#' happen via `vec_cbind()`. In that case, the columns must be
#' identical.
#'
#' @section Casting:
#'
#' Casting to a statically grouped data frame applies the grouping
#' structure of `to` to `x`. While proxying and restoring preserves
#' the group locations, casting does not. The locations must be
#' recomputed on the new data.
#'
#' @noRd
NULL

group_table <- function(x) {
  data <- dplyr::group_data(x)
  data[-length(data)]
}
is_static_grouped_df <- function(x) {
  !dplyr::group_by_drop_default(x)
}

# Reuse group-data but recompute matching rows in `x`
group_data_cast <- function(x, to) {
  gdata <- dplyr::group_data(to)

  gdata <- gdata[-length(gdata)]
  gdata$.rows <- vec_match_all(gdata, x[names(gdata)])

  gdata
}

#' Double dispatch methods for grouped data frames
#' @inheritParams vec_ptype2
#' @export vec_ptype2.grouped_df
#' @export
#' @method vec_ptype2 grouped_df
vec_ptype2.grouped_df <- function(x, y, ...) {
  UseMethod("vec_ptype2.grouped_df", y)
}
#' @export
#' @method vec_ptype2.grouped_df default
vec_ptype2.grouped_df.default <- function(x, y, ...) {
  vec_ptype2.tbl_df(x, y, ...)
}

#' @export
#' @method vec_ptype2.grouped_df grouped_df
vec_ptype2.grouped_df.grouped_df <- function(x, y, ...) {
  ptype <- vec_ptype2(as.data.frame(x), as.data.frame(y))

  x_dynamic <- !is_static_grouped_df(x)
  y_dynamic <- !is_static_grouped_df(y)

  if (x_dynamic && y_dynamic) {
    groups_vars <- union(dplyr::group_vars(x), dplyr::group_vars(y))
    dplyr::grouped_df(ptype, groups_vars, drop = TRUE)
  } else {
    abort("TODO: Combining statically grouped data frames is unimplemented.")
  }
}

#' @export
#' @method vec_ptype2.data.frame grouped_df
vec_ptype2.data.frame.grouped_df <- function(x, y, ...) {
  ptype <- vec_ptype2(x, as.data.frame(y))

  if (is_static_grouped_df(y)) {
    dplyr::new_grouped_df(ptype, groups = dplyr::group_data(y))
  } else {
    dplyr::grouped_df(ptype, vars = dplyr::group_vars(y), drop = TRUE)
  }
}
#' @export
#' @method vec_ptype2.grouped_df data.frame
vec_ptype2.grouped_df.data.frame <- function(x, y, ...) {
  ptype <- vec_ptype2(as.data.frame(x), y)

  if (is_static_grouped_df(x)) {
    dplyr::new_grouped_df(ptype, groups = dplyr::group_data(x))
  } else {
    dplyr::grouped_df(ptype, vars = dplyr::group_vars(x), drop = TRUE)
  }
}

# Can this be inherited from data.frame somehow?
#' @export
#' @method vec_ptype2.tbl_df grouped_df
vec_ptype2.tbl_df.grouped_df <- function(x, y, ...) {
  vec_ptype2.data.frame.grouped_df(x, y, ...)
}

#' @rdname vec_ptype2.grouped_df
#' @inheritParams vec_cast
#' @export vec_cast.grouped_df
#' @export
#' @method vec_cast grouped_df
vec_cast.grouped_df <- function(x, to, ...) {
  UseMethod("vec_cast.grouped_df")
}
#' @export
#' @method vec_cast.grouped_df default
vec_cast.grouped_df.default <- function(x, to, ..., x_arg = "", to_arg = "") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.grouped_df data.frame
vec_cast.grouped_df.data.frame <- function(x, to, ...) {
  # Cast to data frame type first. This ensures `x` contains all
  # columns in `to`. Can this delegation be encoded in a
  # `next_method2()` primitive for double dispatch generics?
  x <- vec_cast(x, as.data.frame(to))

  if (is_static_grouped_df(to)) {
    dplyr::new_grouped_df(x, group_data_cast(x, to))
  } else {
    dplyr::grouped_df(x, dplyr::group_vars(to), drop = TRUE)
  }
}

#' @export
vec_proxy.grouped_df <- function(x, ...) {
  if (is_static_grouped_df(x)) {
    vec_proxy_grouped_df_static(x, ...)
  } else {
    vec_proxy_grouped_df_dynamic(x, ...)
  }
}
#' @export
vec_restore.grouped_df <- function(x, to, ...) {
  if (is_static_grouped_df(to)) {
    vec_restore_grouped_df_static(x, to, ...)
  } else {
    vec_restore_grouped_df_dynamic(x, to, ...)
  }
}

vec_proxy_grouped_df_dynamic <- function(x, ...) {
  groups <- dplyr::group_vars(x)

  # Prevent recursion into `grouped_df` implementations while
  # manipulating the proxy
  class(x) <- "data.frame"

  x[groups] <- map2(groups, x[groups], wrap_group_col)
  x
}
vec_restore_grouped_df_dynamic <- function(x, to, ...) {
  # Prevent recursion into `grouped_df` implementations while
  # manipulating the proxy
  class(x) <- "data.frame"

  groups_ind <- map_lgl(x, is_wrapped_group_col)
  groups_vars <- names(x)[groups_ind]

  x <- as.data.frame(x)
  x[] <- map_if(x, groups_ind, `[[`, 1L)

  # If recomputing groups every time is too slow, perhaps we can cache
  # the groups
  dplyr::grouped_df(
    x,
    groups_vars,
    drop = !is_static_grouped_df(to)
  )
}

wrap_group_col <- function(name, x) {
  new_data_frame(
    list2(!!name := x),
    n = length(x),
    class = "dplyr:::grouped_col"
  )
}
is_wrapped_group_col <- function(x) {
  inherits(x, "dplyr:::grouped_col")
}

vec_proxy_grouped_df_static <- function(x, ...) {
  gdata <- dplyr::group_data(x)
  groups <- vec_group_pos_as_id(gdata$.rows)
  vec_proxy_push_vcols(x, `dplyr::grouped_df_groups` = groups)
}
vec_restore_grouped_df_static <- function(x, to, ...) {
  parts <- vec_proxy_pop_vcols(x, "dplyr::grouped_df_groups")
  proxy <- parts$proxy
  id <- parts$vcols

  # There might be multiple id columns, for instance following a
  # `vec_cbind()`. Should we just assume they are equal for
  # performance?
  if (vec_duplicate_all(as.list(id))) {
    id <- id[[1]]
  } else {
    abort("Internal error: Found incongruent `.id` columns in proxy.")
  }

  gtable_to <- group_table(to)
  rows <- vec_group_id_as_pos(id, vec_size(gtable_to))

  group_data <- c(gtable_to, .rows = list(rows))
  group_data <- new_tibble(group_data, .drop = FALSE)

  dplyr::new_grouped_df(proxy, group_data)
}
