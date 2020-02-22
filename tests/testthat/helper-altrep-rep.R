new_altrep_vctrs_compact_rep_lgl <- function(value, size) {
  value <- vec_assert(value, logical())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_lgl, value, size)
}

new_altrep_vctrs_compact_rep_int <- function(value, size) {
  value <- vec_assert(value, integer())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_int, value, size)
}

new_altrep_vctrs_compact_rep_dbl <- function(value, size) {
  value <- vec_assert(value, double())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_dbl, value, size)
}

new_altrep_vctrs_compact_rep_chr <- function(value, size) {
  value <- vec_assert(value, character())
  size <- vec_cast(vec_cast(size, integer()), double())
  .Call(vctrs_new_altrep_vctrs_compact_rep_chr, value, size)
}

vec_is_altrep_vctrs_compact_rep <- function(x) {
  .Call(vctrs_is_altrep_vctrs_compact_rep, x)
}

vec_is_altrep_vctrs_compact_rep_compact <- function(x) {
  .Call(vctrs_is_altrep_vctrs_compact_rep_compact, x)
}

skip_if_no_altrep <- function() {
  skip_if(getRversion() < "3.5.0", "Testing these ALTREP features requires R 3.5+")
}

skip_if_no_altrep_3_6 <- function() {
  skip_if(getRversion() < "3.6.0", "Testing these ALTREP features requires R 3.6+")
}
