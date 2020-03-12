.rando <- setClass("rando", contains = "numeric", slots = list(.Data = "numeric"))
rando <- function(n = 0) {
  .rando(as.numeric(seq_len(n)))
}

as_rando <- function(x) {
  rando(length(x))
}

setMethod("[", "rando", function(x, i, j, ..., drop = TRUE) {
  new_n <- length(vec_as_location(i, length(x@.Data), names(x@.Data)))
  rando(new_n)
})
