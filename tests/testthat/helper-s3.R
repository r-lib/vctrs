
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
proxy_deref <- function(x) {
  x[[1]]$x
}
scoped_env_proxy <- function(frame = caller_env()) {
  scoped_global_bindings(.frame = frame, vec_proxy.vctrs_proxy = proxy_deref)
}

scoped_no_stringsAsFactors <- function(frame = caller_env()) {
  scoped_options(.frame = frame, stringsAsFactors = FALSE)
}

tibble <- tibble::tibble

scoped_foobar_proxy <- function(frame = caller_env()) {
  scoped_global_bindings(.frame = frame, vec_proxy.vctrs_foobar = identity)
}
