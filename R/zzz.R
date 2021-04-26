# nocov start

on_package_load <- function(pkg, expr) {
  if (isNamespaceLoaded(pkg)) {
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}

.onLoad <- function(libname, pkgname) {
  check_linked_version(pkgname)
  ns <- ns_env("vctrs")

  run_on_load()

  on_package_load("testthat", {
    s3_register("testthat::is_informative_error", "vctrs_error_cast_lossy")
    s3_register("testthat::is_informative_error", "vctrs_error_cast_lossy_dropped")
  })

  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  # Remove once tibble has implemented the methods
  on_package_load("tibble", {
    if (!env_has(ns_env("tibble"), "vec_ptype2.tbl_df.tbl_df")) {
      s3_register("vctrs::vec_ptype2", "tbl_df.tbl_df")
      s3_register("vctrs::vec_ptype2", "tbl_df.data.frame")
      s3_register("vctrs::vec_ptype2", "data.frame.tbl_df")
    }

    if (!env_has(ns_env("tibble"), "vec_cast.tbl_df.tbl_df")) {
      s3_register("vctrs::vec_cast", "tbl_df.tbl_df")
      s3_register("vctrs::vec_cast", "tbl_df.data.frame")
      s3_register("vctrs::vec_cast", "data.frame.tbl_df")
    }
  })

  on_package_load("dplyr", {
    if (!env_has(ns_env("dplyr"), "vec_restore.grouped_df")) {
      s3_register("vctrs::vec_restore", "grouped_df")
    }

    if (!env_has(ns_env("dplyr"), "vec_ptype2.grouped_df.grouped_df")) {
      s3_register("vctrs::vec_ptype2", "grouped_df.grouped_df")
      s3_register("vctrs::vec_ptype2", "grouped_df.data.frame")
      s3_register("vctrs::vec_ptype2", "grouped_df.tbl_df")
      s3_register("vctrs::vec_ptype2", "data.frame.grouped_df")
      s3_register("vctrs::vec_ptype2", "tbl_df.grouped_df")
    }

    if (!env_has(ns_env("dplyr"), "vec_cast.grouped_df.grouped_df")) {
      s3_register("vctrs::vec_cast", "grouped_df.grouped_df")
      s3_register("vctrs::vec_cast", "grouped_df.data.frame")
      s3_register("vctrs::vec_cast", "grouped_df.tbl_df")
      s3_register("vctrs::vec_cast", "data.frame.grouped_df")
      s3_register("vctrs::vec_cast", "tbl_df.grouped_df")
    }

    if (!env_has(ns_env("dplyr"), "vec_restore.rowwise_df")) {
      s3_register("vctrs::vec_restore", "rowwise_df")
    }

    if (!env_has(ns_env("dplyr"), "vec_ptype2.rowwise_df.rowwise_df")) {
      s3_register("vctrs::vec_ptype2", "rowwise_df.rowwise_df")
      s3_register("vctrs::vec_ptype2", "rowwise_df.data.frame")
      s3_register("vctrs::vec_ptype2", "rowwise_df.tbl_df")
      s3_register("vctrs::vec_ptype2", "data.frame.rowwise_df")
      s3_register("vctrs::vec_ptype2", "tbl_df.rowwise_df")
    }

    if (!env_has(ns_env("dplyr"), "vec_cast.rowwise_df.rowwise_df")) {
      s3_register("vctrs::vec_cast", "rowwise_df.rowwise_df")
      s3_register("vctrs::vec_cast", "rowwise_df.data.frame")
      s3_register("vctrs::vec_cast", "rowwise_df.tbl_df")
      s3_register("vctrs::vec_cast", "data.frame.rowwise_df")
      s3_register("vctrs::vec_cast", "tbl_df.rowwise_df")
    }
  })

  on_package_load("sf", {
    import_from("sf", sf_deps, env = sf_env)

    if (!env_has(ns_env("sf"), "vec_restore.sf")) {
      s3_register("vctrs::vec_proxy", "sf")
      s3_register("vctrs::vec_restore", "sf")
    }
    if (!env_has(ns_env("sf"), "vec_ptype2.sf.sf")) {
      s3_register("vctrs::vec_ptype2", "sf.sf")
      s3_register("vctrs::vec_ptype2", "sf.data.frame")
      s3_register("vctrs::vec_ptype2", "data.frame.sf")
      s3_register("vctrs::vec_ptype2", "sf.tbl_df")
      s3_register("vctrs::vec_ptype2", "tbl_df.sf")
      s3_register("vctrs::vec_cast", "sf.sf")
      s3_register("vctrs::vec_cast", "sf.data.frame")
      s3_register("vctrs::vec_cast", "data.frame.sf")
    }
  })

  utils::globalVariables("vec_set_attributes")

  # Prevent two copies from being made by `attributes(x) <- attrib` on R < 3.6.0
  if (getRversion() >= '3.6.0') {
    vec_set_attributes <- function(x, attrib) {
      attributes(x) <- attrib
      x
    }
  } else {
    vec_set_attributes <- function(x, attrib) {
      .Call(vctrs_set_attributes, x, attrib)
    }
  }

  env_bind(ns, vec_set_attributes = vec_set_attributes)

  .Call(vctrs_init_library, ns_env())
}

# nocov end
