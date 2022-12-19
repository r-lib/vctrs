vec_set_intersect <- function(x,
                              y,
                              ...,
                              ptype = NULL,
                              x_arg = "x",
                              y_arg = "y",
                              error_call = current_env()) {
  check_dots_empty0(...)
  .Call(ffi_vec_set_intersect, x, y, ptype, environment())
}
