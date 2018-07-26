recycle <- function(...) {
  args <- list2(...)

  shape <- reduce(args, vecshape_max)
  map(args, vecshape_coerce, shape = shape)
}
