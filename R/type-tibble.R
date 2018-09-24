new_tibble <- function(x, n) {
  new_data_frame(x, n, class = c("tbl_df", "tbl"))
}


# Coercion ----------------------------------------------------------------


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

