vec_fill <- function(x, direction = c("down", "up", "downup", "updown")) {
  .Call(vctrs_fill, x, direction)
}
