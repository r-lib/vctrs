#' @rdname df_ptype2
#' @export
tib_ptype2 <- function(x,
                       y,
                       ...,
                       x_arg = "",
                       y_arg = "",
                       call = caller_env()) {
  .Call(
    ffi_tib_ptype2,
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg,
    frame = environment()
  )
}
#' @rdname df_ptype2
#' @export
tib_cast <- function(x,
                     to,
                     ...,
                     x_arg = "",
                     to_arg = "",
                     call = caller_env()) {
  .Call(
    ffi_tib_cast,
    x = x,
    to = to,
    x_arg = x_arg,
    to_arg = to_arg,
    frame = environment()
  )
}

df_as_tibble <- function(df) {
  class(df) <- c("tbl_df", "tbl", "data.frame")
  df
}

# Conditionally registered in .onLoad()

vec_ptype2_tbl_df_tbl_df <- function(x, y, ...) {
  vec_ptype2_dispatch_native(x, y, ...)
}
vec_ptype2_tbl_df_data.frame <- function(x, y, ...) {
  vec_ptype2_dispatch_native(x, y, ...)
}
vec_ptype2_data.frame_tbl_df <- function(x, y, ...) {
  vec_ptype2_dispatch_native(x, y, ...)
}

vec_cast_tbl_df_tbl_df <- function(x, to, ...) {
  vec_cast_dispatch_native(x, to, ...)
}
vec_cast_data.frame_tbl_df <- function(x, to, ...) {
  vec_cast_dispatch_native(x, to, ...)
}
vec_cast_tbl_df_data.frame <- function(x, to, ...) {
  vec_cast_dispatch_native(x, to, ...)
}
