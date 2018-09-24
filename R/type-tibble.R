new_tibble <- function(x, n) {
  new_data_frame(x, n, class = c("tbl_df", "tbl"))
}


# Coercion ----------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.tbl_df
#' @method vec_type2 tbl_df
#' @export
vec_type2.tbl_df     <- function(x, y) UseMethod("vec_type2.tbl_df", y)

#' @method vec_type2.tbl_df data.frame
#' @export
vec_type2.tbl_df.data.frame <- function(x, y) {
  df <- df_col_type2(x, y)
  new_tibble(df, n = 0)
}

#' @method vec_type2.data.frame tbl_df
#' @export
vec_type2.data.frame.tbl_df <- function(x, y) {
  df <- df_col_type2(x, y)
  new_tibble(df, n = 0)
}

#' @method vec_type2.tbl_df default
#' @export
vec_type2.tbl_df.default <- function(x, y) stop_incompatible_type(x, y)


# Cast --------------------------------------------------------------------

#' @rdname vec_cast
#' @export vec_cast.tbl_df
#' @method vec_cast tbl_df
#' @export
vec_cast.tbl_df <- function(x, to) {
  UseMethod("vec_cast.tbl_df")
}
#' @export
#' @method vec_cast.tbl_df NULL
vec_cast.tbl_df.NULL <- function(x, to) {
  x
}
#' @export
#' @method vec_cast.tbl_df data.frame
vec_cast.tbl_df.data.frame <- function(x, to) {
  tibble::as_tibble(df_col_cast(x, to))
}
#' @export
#' @method vec_cast.data.frame default
vec_cast.tbl_df.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

