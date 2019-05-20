# nocov start
r_version_at_least_3.6.0 <- NULL

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")

  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  r_version_at_least_3.6.0 <<- getRversion() >= '3.6.0'

  .Call(vctrs_init, ns_env())
}

# nocov end
