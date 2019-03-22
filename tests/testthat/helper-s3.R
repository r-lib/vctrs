
foobar <- function(x = list()) structure(x, class = "vctrs_foobar")

unrownames <- function(x) {
  row.names(x) <- NULL
  x
}

scoped_global_bindings <- function(..., .frame = caller_env()) {
  scoped_bindings(..., .env = global_env(), .frame = .frame)
}

new_proxy <- function(x) {
  structure(list(env(x = x)), class = "vctrs_proxy")
}
