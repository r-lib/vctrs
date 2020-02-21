new_altrep_vctrs_compact_rep_lgl <- function(value, size) {
  value <- vec_cast(value, logical())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_lgl, value, size)
}

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

new_altrep_vctrs_compact_rep_chr <- function(value, size) {
  value <- vec_cast(value, character())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_chr, value, size)
}

is_altrep_vctrs_compact_rep_compact <- function(x) {
  .Call(vctrs_is_altrep_vctrs_compact_rep_compact, x)
}

missing_altrep <- function() {
  getRversion() < "3.5.0"
}

missing_altrep_3_6 <- function() {
  getRversion() < "3.6.0"
}
