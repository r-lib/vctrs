chr_rle <- function(...) {
  new_chr_rle(c(...))
}

new_chr_rle <- function(x) {
  stopifnot(is.integer(x), is_named(x))
  .Call(vctrs_altrep_rle_Make, x)
}

chr_rle_is_materialized <- function(x) {
  .Call(vctrs_altrep_rle_is_materialized, x)
}
