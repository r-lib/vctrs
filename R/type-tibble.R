# Coercion ----------------------------------------------------------------

#' @rdname vec_type2
#' @export vec_type2.tbl_df
#' @method vec_type2 tbl_df
#' @export
vec_type2.tbl_df     <- function(x, y) UseMethod("vec_type2.tbl_df", y)

#' @method vec_type2.tbl_df data.frame
#' @export
vec_type2.tbl_df.data.frame <- function(x, y) df_col_type2(x, y)

#' @method vec_type2.data.frame tbl_df
#' @export
vec_type2.data.frame.tbl_df <- function(x, y) vec_restore(df_col_type2(x, y), y)

#' @method vec_type2.tbl_df default
#' @export
vec_type2.tbl_df.default <- function(x, y) stop_incompatible_type(x, y)
