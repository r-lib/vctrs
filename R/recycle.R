recycle <- function(...) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  shape <- max.vecshape(!!!args)
  map(args, vecshape_coerce, shape = shape)
}
