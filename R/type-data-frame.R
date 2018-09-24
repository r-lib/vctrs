# TODO: consider names and export so that they can be used by
# data frame subclasses in other packages

data_frame <- function(...) {
  cols <- tibble::set_tidy_names(list(...))
  new_data_frame(cols)
}

new_data_frame <- function(x = list(), n = df_length(x), ...,  class = character()) {
  n <- as.integer(n)

  structure(
    x,
    ...,
    class = c(class, "data.frame"),
    row.names = .set_row_names(n)
  )
}

df_length <- function(x) {
  if (length(x) > 0) {
    n <- length(x[[1]])
  } else {
    n <- 0L
  }
}

df_col_type2 <- function(x, y) {
  x <- vec_data(x)
  y <- vec_data(y)
  names <- set_partition(names(x), names(y))

  # Find types
  if (length(names$both) > 0) {
    common_types <- map2(x[names$both], y[names$both], vec_type2)
  } else {
    common_types <- list()
  }
  only_x_types <- map(x[names$only_x], vec_subset, 0L)
  only_y_types <- map(y[names$only_y], vec_subset, 0L)

  # Combine and restore order
  out <- c(common_types, only_x_types, only_y_types)
  out[c(names(x), names$only_y)]
}

df_col_cast <- function(x, to) {
  n <- vec_length(x)
  x <- vec_data(x)

  # Coerce common columns
  common <- intersect(names(x), names(to))
  x[common] <- map2(x[common], to[common], vec_cast)

  # Add new columns
  from_type <- setdiff(names(to), names(x))
  x[from_type] <- map(to[from_type], vec_na, n = n)

  # Warn about dropped columns
  dropped <- setdiff(names(x), names(to))
  if (length(dropped) > 0 ) {
    warn_lossy_cast(
      x, to,
      details = inline_list("Dropped variables: ", dropped, quote = "`")
    )
  }

  x[names(to)]
}
