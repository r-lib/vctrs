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
  # FIXME: Prevent inheritance
  vec_ptype2.data.frame(x, y, ...)
}

vec_ptype2.tbl_df.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  .Call(
    vctrs_tibble_ptype2,
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg
  )
}
vec_ptype2.data.frame.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  .Call(
    vctrs_tibble_ptype2,
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg
  )
}
