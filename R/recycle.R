recycle <- function(...) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  shape <- max.vec_shape(!!!args)
  map(args, vecshape_coerce, shape = shape)
}
