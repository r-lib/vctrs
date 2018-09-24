new_factor <- function(x = integer(), levels = character(), ..., class = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))

  structure(
    x,
    levels = levels,
    ...,
    class = c(class, "factor")
  )
}

new_ordered <- function(x = integer(), levels = character()) {
  new_factor(x = x, levels = levels, class = "ordered")
}

levels_union <- function(x, y) {
  union(levels(x), levels(y))
}
