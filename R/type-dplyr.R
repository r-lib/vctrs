
# All methods in this file are conditionally registered in .onLoad()

vec_restore.grouped_df <- function(x, to, ...) {
  check_drop(to)
  dplyr::grouped_df(x, dplyr::group_vars(to), drop = TRUE)
}

check_drop <- function(x) {
  if (is_false(dplyr::group_by_drop_default(x))) {
    abort("Grouped data frames created with `drop = FALSE` are unsupported in vctrs.")
  }
}
