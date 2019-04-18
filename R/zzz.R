# nocov start
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")

  s3_register("pillar::pillar_shaft", "vctrs_vctr")
  s3_register("tibble::type_sum", "vctrs_vctr")
  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  ns <- ns_env("vctrs")

  if (getRversion() < "3.5.0") {
    env_bind(ns,
      ...length = function() .Call(vctrs_dots_length, parent.frame())
    )
  }

  utils::globalVariables(c(
    "vec_type2_workaround",
    "vec_cast_workaround",
    "vec_slice_workaround",
    "vec_restore_workaround"
  ))

  # Work around "invalid generic in usemethod" error on R 3.1
  if (getRversion() < "3.2.0") {
    env_bind(ns,
      vec_type2_workaround = vec_type2_dispatch,
      vec_type2_dispatch = function(...) vec_type2_workaround(...),
      vec_cast_workaround = vec_cast_dispatch,
      vec_cast_dispatch = function(...) vec_cast_workaround(...),
      vec_restore_workaround = vec_restore_dispatch,
      vec_restore_dispatch = function(...) vec_restore_workaround(...)
    )
  }

  .Call(vctrs_init, ns)
}

# nocov end
