new_altrep_vctrs_compact_intrep <- function(value, size) {
  value <- vec_cast(value, integer())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_intrep, value, size)
}
