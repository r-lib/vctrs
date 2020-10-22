vec_fill <- function(x, direction = c("down", "up"), leading = FALSE) {
  .Call(vctrs_fill, x, direction, leading)
}
