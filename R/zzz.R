# nocov start

.onLoad <- function(libname, pkgname) {
  check_linked_version(pkgname)
  ns <- ns_env("vctrs")

  run_on_load()

  on_package_load("testthat", {
    s3_register("testthat::is_informative_error", "vctrs_error_cast_lossy", is_informative_error_vctrs_error_cast_lossy)
    s3_register("testthat::is_informative_error", "vctrs_error_cast_lossy_dropped", is_informative_error_vctrs_error_cast_lossy_dropped)
  })

  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  # Remove once tibble has implemented the methods
  on_package_load("tibble", {
    if (!env_has(ns_env("tibble"), "vec_ptype2.tbl_df.tbl_df")) {
      s3_register("vctrs::vec_ptype2", "tbl_df.tbl_df", vec_ptype2_tbl_df_tbl_df)
      s3_register("vctrs::vec_ptype2", "tbl_df.data.frame", vec_ptype2_tbl_df_data.frame)
      s3_register("vctrs::vec_ptype2", "data.frame.tbl_df", vec_ptype2_data.frame_tbl_df)
    }

    if (!env_has(ns_env("tibble"), "vec_cast.tbl_df.tbl_df")) {
      s3_register("vctrs::vec_cast", "tbl_df.tbl_df", vec_cast_tbl_df_tbl_df)
      s3_register("vctrs::vec_cast", "tbl_df.data.frame", vec_cast_tbl_df_data.frame)
      s3_register("vctrs::vec_cast", "data.frame.tbl_df", vec_cast_data.frame_tbl_df)
    }
  })

  on_package_load("dplyr", {
    if (!env_has(ns_env("dplyr"), "vec_restore.grouped_df")) {
      s3_register("vctrs::vec_restore", "grouped_df", vec_restore_grouped_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_ptype2.grouped_df.grouped_df")) {
      s3_register("vctrs::vec_ptype2", "grouped_df.grouped_df", vec_ptype2_grouped_df_grouped_df)
      s3_register("vctrs::vec_ptype2", "grouped_df.data.frame", vec_ptype2_grouped_df_data.frame)
      s3_register("vctrs::vec_ptype2", "grouped_df.tbl_df", vec_ptype2_grouped_df_tbl_df)
      s3_register("vctrs::vec_ptype2", "data.frame.grouped_df", vec_ptype2_data.frame_grouped_df)
      s3_register("vctrs::vec_ptype2", "tbl_df.grouped_df", vec_ptype2_tbl_df_grouped_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_cast.grouped_df.grouped_df")) {
      s3_register("vctrs::vec_cast", "grouped_df.grouped_df", vec_cast_grouped_df_grouped_df)
      s3_register("vctrs::vec_cast", "grouped_df.data.frame", vec_cast_grouped_df_data.frame)
      s3_register("vctrs::vec_cast", "grouped_df.tbl_df", vec_cast_grouped_df_tbl_df)
      s3_register("vctrs::vec_cast", "data.frame.grouped_df", vec_cast_data.frame_grouped_df)
      s3_register("vctrs::vec_cast", "tbl_df.grouped_df", vec_cast_tbl_df_grouped_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_restore.rowwise_df")) {
      s3_register("vctrs::vec_restore", "rowwise_df", vec_restore_rowwise_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_ptype2.rowwise_df.rowwise_df")) {
      s3_register("vctrs::vec_ptype2", "rowwise_df.rowwise_df", vec_ptype2_rowwise_df_rowwise_df)
      s3_register("vctrs::vec_ptype2", "rowwise_df.data.frame", vec_ptype2_rowwise_df_data.frame)
      s3_register("vctrs::vec_ptype2", "rowwise_df.tbl_df", vec_ptype2_rowwise_df_tbl_df)
      s3_register("vctrs::vec_ptype2", "data.frame.rowwise_df", vec_ptype2_data.frame_rowwise_df)
      s3_register("vctrs::vec_ptype2", "tbl_df.rowwise_df", vec_ptype2_tbl_df_rowwise_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_cast.rowwise_df.rowwise_df")) {
      s3_register("vctrs::vec_cast", "rowwise_df.rowwise_df", vec_cast_rowwise_df_rowwise_df)
      s3_register("vctrs::vec_cast", "rowwise_df.data.frame", vec_cast_rowwise_df_data.frame)
      s3_register("vctrs::vec_cast", "rowwise_df.tbl_df", vec_cast_rowwise_df_tbl_df)
      s3_register("vctrs::vec_cast", "data.frame.rowwise_df", vec_cast_data.frame_rowwise_df)
      s3_register("vctrs::vec_cast", "tbl_df.rowwise_df", vec_cast_tbl_df_rowwise_df)
    }
  })

  on_package_load("sf", {
    import_from("sf", sf_deps, env = sf_env)

    if (!env_has(ns_env("sf"), "vec_restore.sf")) {
      s3_register("vctrs::vec_proxy", "sf", vec_proxy_sf)
      s3_register("vctrs::vec_restore", "sf", vec_restore_sf)
    }
    if (!env_has(ns_env("sf"), "vec_ptype2.sf.sf")) {
      s3_register("vctrs::vec_ptype2", "sf.sf", vec_ptype2_sf_sf)
      s3_register("vctrs::vec_ptype2", "sf.data.frame", vec_ptype2_sf_data.frame)
      s3_register("vctrs::vec_ptype2", "data.frame.sf", vec_ptype2_data.frame_sf)
      s3_register("vctrs::vec_ptype2", "sf.tbl_df", vec_ptype2_sf_tbl_df)
      s3_register("vctrs::vec_ptype2", "tbl_df.sf", vec_ptype2_tbl_df_sf)
      s3_register("vctrs::vec_cast", "sf.sf", vec_cast_sf_sf)
      s3_register("vctrs::vec_cast", "sf.data.frame", vec_cast_sf_data.frame)
      s3_register("vctrs::vec_cast", "data.frame.sf", vec_cast_data.frame_sf)
    }
    if (!env_has(ns_env("sf"), "vec_proxy_order.sfc")) {
      s3_register("vctrs::vec_proxy_order", "sfc", vec_proxy_order_sfc)
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
