vec_duplicated <- function(x) {
  .Call(vctrs_duplicated, x)
}

vec_count <- function(x) {
  count <- .Call(vctrs_count, x)
  data.frame(x = vec_subset(x, count$idx), count = count$count)
}

vec_id <- function(x) {
  .Call(vctrs_id, x)
}
