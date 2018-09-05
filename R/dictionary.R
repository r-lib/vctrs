vec_duplicated <- function(x) {
  .Call(vctrs_duplicated, x)
}

vec_count <- function(x) {
  count <- .Call(vctrs_count, x)
  df <- data.frame(key = 0, count = count$val)
  df$key <- count$key
  df
}

vec_id <- function(x) {
  .Call(vctrs_id, x)
}

vec_n_distinct <- function(x) {
  .Call(vctrs_n_distinct, x)
}
