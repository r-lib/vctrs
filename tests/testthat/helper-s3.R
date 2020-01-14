
foobar <- function(x = list()) structure(x, class = "vctrs_foobar")

unrownames <- function(x) {
  row.names(x) <- NULL
  x
}

local_methods <- function(..., .frame = caller_env()) {
  local_bindings(..., .env = global_env(), .frame = .frame)
}
local_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_proxy.vctrs_proxy = function(x, ...) proxy_deref(x),
    vec_restore.vctrs_proxy = function(x, to, ...) new_proxy(x),

    vec_ptype2.vctrs_proxy = function(x, y, ...) UseMethod("vec_ptype2.vctrs_proxy", y),
    vec_ptype2.vctrs_proxy.vctrs_proxy = function(x, y, ...) new_proxy(vec_ptype(proxy_deref(x))),

    vec_cast.vctrs_proxy = function(x, to, ...) UseMethod("vec_cast.vctrs_proxy"),
    vec_cast.vctrs_proxy.default = function(x, to, ...) stop_incompatible_cast(x, to),
    vec_cast.vctrs_proxy.vctrs_proxy = function(x, to, ...) x
  )
}


new_proxy <- function(x) {
  structure(list(env(x = x)), class = "vctrs_proxy")
}
proxy_deref <- function(x) {
  x[[1]]$x
}
local_env_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_proxy.vctrs_proxy = proxy_deref,
    vec_restore.vctrs_proxy = function(x, ...) new_proxy(x)
  )
}

local_no_stringsAsFactors <- function(frame = caller_env()) {
  local_options(.frame = frame, stringsAsFactors = FALSE)
}

tibble <- function(...) {
  tibble::tibble(...)
}

local_foobar_proxy <- function(frame = caller_env()) {
  local_methods(.frame = frame, vec_proxy.vctrs_foobar = identity)
}

subclass <- function(x) {
  class(x) <- c("vctrs_foo", "vctrs_foobar", class(x))
  x
}


# Subclass promoted to logical
new_lgl_subtype <- function(x) {
  stopifnot(is_logical(x))
  structure(x, class = "vctrs_lgl_subtype")
}
local_lgl_subtype <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_ptype2.vctrs_lgl_subtype = function(x, y, ...) UseMethod("vec_ptype2.vctrs_lgl_subtype", y),
    vec_ptype2.vctrs_lgl_subtype.default = function(x, y, ...) vec_default_ptype2(x, y),
    vec_ptype2.vctrs_lgl_subtype.vctrs_lgl_subtype = function(x, y, ...) x,
    vec_ptype2.vctrs_lgl_subtype.logical = function(x, y, ...) y,
    vec_ptype2.logical.vctrs_lgl_subtype = function(x, y, ...) x,

    vec_cast.vctrs_lgl_subtype = function(x, to, ...) UseMethod("vec_cast.vctrs_lgl_subtype"),
    vec_cast.vctrs_lgl_subtype.default = function(x, to, ...) stop_incompatible_cast(x, to),
    vec_cast.vctrs_lgl_subtype.vctrs_lgl_subtype = function(x, to, ...) x,
    vec_cast.vctrs_lgl_subtype.logical = function(x, to, ...) new_lgl_subtype(x),
    vec_cast.logical.vctrs_lgl_subtype = function(x, to, ...) unstructure(x)
  )
}
with_lgl_subtype <- function(expr) {
  local_lgl_subtype()
  expr
}

# Logical promoted to subclass
new_lgl_supertype <- function(x) {
  stopifnot(is_logical(x))
  structure(x, class = "vctrs_lgl_supertype")
}
local_lgl_supertype <- function(frame = caller_env()) {
  local_methods(.frame = frame,
    vec_ptype2.vctrs_lgl_supertype = function(x, y, ...) UseMethod("vec_ptype2.vctrs_lgl_supertype", y),
    vec_ptype2.vctrs_lgl_supertype.default = function(x, y, ...) vec_default_ptype2(x, y),
    vec_ptype2.vctrs_lgl_supertype.vctrs_lgl_supertype = function(x, y, ...) x,
    vec_ptype2.vctrs_lgl_supertype.logical = function(x, y, ...) x,
    vec_ptype2.logical.vctrs_lgl_supertype = function(x, y, ...) y,

    vec_cast.vctrs_lgl_supertype = function(x, to, ...) UseMethod("vec_cast.vctrs_lgl_supertype"),
    vec_cast.vctrs_lgl_supertype.default = function(x, to, ...) stop_incompatible_cast(x, to),
    vec_cast.vctrs_lgl_supertype.vctrs_lgl_supertype = function(x, to, ...) x,
    vec_cast.vctrs_lgl_supertype.logical = function(x, to, ...) new_lgl_subtype(x),
    vec_cast.logical.vctrs_lgl_supertype = function(x, to, ...) unstructure(x)
  )
}
with_lgl_supertype <- function(expr) {
  local_lgl_supertype()
  expr
}
