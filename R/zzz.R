# nocov start
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")

  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  .Call(vctrs_init, ns_env())
}

# nocov end
