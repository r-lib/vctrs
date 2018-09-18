cmp <- function(x, y) (x > y) - (x < y)

vec_compare <- function(x, y, na_equal = FALSE, .ptype = NULL) {
  args <- vec_recycle(x, y)
  args <- vec_coerce(!!!args, .ptype = .ptype)
  .Call(vctrs_compare, vec_proxy_equality(args[[1]]), vec_proxy_equality(args[[2]]), na_equal)
}
