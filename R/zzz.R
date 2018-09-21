# nocov start
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")

  vec_method_register("pillar::pillar_shaft", "vctrs_vctr")
  vec_method_register("tibble::type_sum", "vctrs_vctr")
}

# nocov end
