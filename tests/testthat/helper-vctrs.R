
testthat_import_from <- function(ns, names, env = caller_env()) {
  skip_if_not_installed(ns)
  import_from(ns, names, env = env)
}

vec_ptype2_fallback <- function(x, y, ...) {
  vec_ptype2_params(x, y, ..., df_fallback = DF_FALLBACK_warn)
}
vec_ptype_common_df_fallback <- function(..., .ptype = NULL) {
  vec_ptype_common_params(
    ...,
    .ptype = .ptype,
    .df_fallback = DF_FALLBACK_warn
  )
}

shaped_int <- function(...) {
  array(NA_integer_, c(...))
}
