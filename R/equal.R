
vec_equal <- function(x, y, .ptype = NULL) {
  args <- vec_recycle(x, y)
  args <- vec_coerce(!!!args, .ptype = .ptype)
  .Call(vctrs_equal, vec_proxy_equality(args[[1]]), vec_proxy_equality(args[[2]]))
}

obj_equal <- function(x, y) {
  .Call(vctrs_equal_object, x, y)
}
