new_altrep_vctrs_compact_rep_int <- function(value, size) {
  value <- vec_cast(value, integer())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_int, value, size)
}

new_altrep_vctrs_compact_rep_dbl <- function(value, size) {
  value <- vec_cast(value, double())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_dbl, value, size)
}
