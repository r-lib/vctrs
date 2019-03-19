
foobar <- function(x = list()) structure(x, class = "vctrs_foobar")

unrownames <- function(x) {
  row.names(x) <- NULL
  x
}
