# TODO: consider names and export so that they can be used by
# data frame subclasses in other packages

df_empty <- function(cols, subclass = NULL) {
  structure(
    cols,
    class = c(subclass, "data.frame"),
    row.names = .set_row_names(0L)
  )
}

df_col_type2 <- function(x, y) {
  common <- intersect(names(x), names(y))
  only_x <- setdiff(names(x), names(y))
  only_y <- setdiff(names(y), names(x))

  # Find types
  if (length(common) > 0) {
    common_types <- map2(x[common], y[common], vec_type2)
  } else {
    common_types <- list()
  }
  only_x_types <- map(x[only_x], vec_subset, 0L)
  only_y_types <- map(y[only_y], vec_subset, 0L)

  # Combine and restore order
  out <- c(common_types, only_x_types, only_y_types)
  out[c(names(x), setdiff(names(y), names(x)))]
}

df_col_cast <- function(x, to) {
  # Coerce common columns
  common <- intersect(names(x), names(to))
  x[common] <- map2(x[common], to[common], vec_cast)

  # Add new columns
  from_type <- setdiff(names(to), names(x))
  x[from_type] <- map(to[from_type], vec_na, n = vec_length(x))

  # Warn about dropped columns
  dropped <- setdiff(names(x), names(to))
  if (length(dropped) > 0 ) {
    warn_cast_lossy_dataframe(x, to, dropped)
  }

  x[names(to)]
}
