# nocov start
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "strrep")

  s3_register("pillar::pillar_shaft", "vctrs_vctr")
  s3_register("tibble::type_sum", "vctrs_vctr")
  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  utils::globalVariables(c(
    "vec_is_vector_workaround",
    "vec_type2_workaround",
    "vec_cast_workaround",
    "vec_slice_workaround"
  ))

  # Work around "invalid generic in usemethod" error on R 3.1
  if (getRversion() < "3.2.0") {
    env_bind(ns_env("vctrs"),
      vec_is_vector_workaround = vec_is_vector_dispatch,
      vec_is_vector_dispatch = function(...) vec_is_vector_workaround(...),
      vec_type2_workaround = vec_type2_dispatch,
      vec_type2_dispatch = function(...) vec_type2_workaround(...),
      vec_cast_workaround = vec_cast_dispatch,
      vec_cast_dispatch = function(...) vec_cast_workaround(...),
      vec_slice_workaround = vec_slice_dispatch,
      vec_slice_dispatch = function(...) vec_slice_workaround(...),
      vec_restore_workaround = vec_restore_dispatch,
      vec_restore_dispatch = function(...) vec_restore_workaround(...)
    )
  }

  .Call(vctrs_init, environment())
}

# nocov end
