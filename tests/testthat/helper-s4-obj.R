.zando <- setClass("zando", slots = list(x = "numeric"))
zando <- function(n = 0) {
  .zando(x = as.numeric(seq_len(n)))
}

as_zando <- function(x) {
  zando(length(x))
}

setMethod("[", "zando", function(x, i, j, ..., drop = TRUE) {
  new_n <- length(vec_as_location(i, length(x@n), names(x@n)))
  zando(new_n)
})

vec_proxy.zando <- function(x, ...) {
  x
}

s3_register("vctrs::vec_proxy", "zando")
