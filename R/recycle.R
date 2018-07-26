recycle <- function(...) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())

  shapes <- map(args, as_shape)
  shape <- invoke(max, shapes)

  map(args, vecshape_coerce, shape = shape)
}
