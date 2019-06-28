# Coercion ----------------------------------------------------------------

#' @rdname vec_ptype2
#' @export vec_ptype2.tbl_df
#' @method vec_ptype2 tbl_df
#' @export
vec_ptype2.tbl_df <- function(x, y, ...) UseMethod("vec_ptype2.tbl_df", y)

#' @method vec_ptype2.tbl_df data.frame
#' @export
vec_ptype2.tbl_df.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  .Call(vctrs_type2_df_df, x, y, x_arg, y_arg)
}
#' @method vec_ptype2.data.frame tbl_df
#' @export
vec_ptype2.data.frame.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  .Call(vctrs_type2_df_df, x, y, x_arg, y_arg)
}

#' @method vec_ptype2.tbl_df default
#' @export
vec_ptype2.tbl_df.default <- function(x, y, ...) {
  # FIXME: Do we need some sort of `next_vec_type2()`?
  vec_ptype2.data.frame(x, y)
}
