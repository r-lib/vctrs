vec_rank <- function(x,
                     ...,
                     ties = c("min", "max", "sequential", "dense"),
                     na_propagate = FALSE,
                     na_value = "largest",
                     nan_distinct = FALSE,
                     chr_transform = NULL) {
  if (!missing(...)) {
    ellipsis::check_dots_empty()
  }
  .Call(vctrs_rank, x, ties, na_propagate, na_value, nan_distinct, chr_transform)
}
