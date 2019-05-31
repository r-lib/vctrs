# Coercion ----------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.tbl_df
#' @method vec_type2 tbl_df
#' @export
vec_type2.tbl_df <- function(x, y, ...) UseMethod("vec_type2.tbl_df", y)

#' @method vec_type2.tbl_df data.frame
#' @export
vec_type2.tbl_df.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  .Call(vctrs_type2_df_df, x, y, x_arg, y_arg)
}
#' @method vec_type2.data.frame tbl_df
#' @export
vec_type2.data.frame.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  .Call(vctrs_type2_df_df, x, y, x_arg, y_arg)
}

#' @method vec_type2.tbl_df default
#' @export
vec_type2.tbl_df.default <- function(x, y, ...) {
  # FIXME: Do we need some sort of `next_vec_type2()`?
  vec_type2.data.frame(x, y)
}
