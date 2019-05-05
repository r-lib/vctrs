# nocov start
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, c("strrep", "...length"))

  s3_register("pillar::pillar_shaft", "vctrs_vctr")
  s3_register("pillar::type_sum", "vctrs_vctr")
  s3_register("pillar::type_sum", "vctrs_unspecified")
  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  .Call(vctrs_init, ns_env())
}

# nocov end
