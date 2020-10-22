vec_fill <- function(x,
                     direction = c("down", "up"),
                     leading = FALSE,
                     max_gap = NULL) {
  .Call(vctrs_fill, x, direction, leading, max_gap)
}
