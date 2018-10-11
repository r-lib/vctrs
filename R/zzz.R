# nocov start
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")

  s3_register("pillar::pillar_shaft", "vctrs_vctr")
  s3_register("tibble::type_sum", "vctrs_vctr")
  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")
}

# nocov end
