repeated <- function(x, n = 0) {
  new_repeated(
    rep(list(vec_na(x)), n),
    vec_subset(x, 0L)
  )
}

# as_repeated needs to derive and/or check
# need function that constructs from individual components

new_repeated <- function(x, prototype) {
  stopifnot(is.list(x))
  stopifnot(vec_length(prototype) == 0)

  structure(
    x,
    prototype = prototype,
    class = "repeated"
  )
}

vec_type.repeated <- function(x) {
  paste0("repeated<", vec_type(attr(x, "prototype")), ">")
}

`[.repeated` <- function(x, ...) {
  new_repeated(x, attr(x, "prototype"))
}

# TODO: methods for [[<-, [<- and $<- that coerce their inputs
