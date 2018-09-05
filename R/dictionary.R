vec_duplicated <- function(x) {
  .Call(vctrs_duplicated, x)
}

vec_unique <- function(x) {
  vec_subset(x, which(!vec_duplicated(x)))
}

vec_n_distinct <- function(x) {
  .Call(vctrs_n_distinct, x)
}

vec_count <- function(x, order = TRUE) {
  kv <- .Call(vctrs_count, x)

  df <- data.frame(key = 0, count = kv$val)
  df$key <- vec_subset(x, kv$key) # might be a dataframe

  if (order) {
    df <- df[order(kv$key), , drop = FALSE]
    rownames(df) <- NULL
  }

  df
}

vec_id <- function(x) {
  .Call(vctrs_id, x)
}

