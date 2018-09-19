
vec_equal <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  args <- vec_recycle(x, y)
  args <- vec_coerce(!!!args, .ptype = .ptype)
  .Call(
    vctrs_equal,
    vec_proxy_equality(args[[1]]),
    vec_proxy_equality(args[[2]]),
    na_equal
  )
}

obj_equal <- function(x, y) {
  .Call(vctrs_equal_object, x, y)
}
