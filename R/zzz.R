# nocov start

.onLoad <- function(libname, pkgname) {
  check_linked_version(pkgname)
  ns <- ns_env("vctrs")

  run_on_load()

  s3_register("generics::as.factor", "vctrs_vctr")
  s3_register("generics::as.ordered", "vctrs_vctr")
  s3_register("generics::as.difftime", "vctrs_vctr")

  # Remove once tibble has implemented the methods
  on_package_load("tibble", {
    if (!env_has(ns_env("tibble"), "vec_ptype2.tbl_df.tbl_df")) {
      s3_register(
        "vctrs::vec_ptype2",
        "tbl_df.tbl_df",
        vec_ptype2_tbl_df_tbl_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "tbl_df.data.frame",
        vec_ptype2_tbl_df_data.frame
      )
      s3_register(
        "vctrs::vec_ptype2",
        "data.frame.tbl_df",
        vec_ptype2_data.frame_tbl_df
      )
    }

    if (!env_has(ns_env("tibble"), "vec_cast.tbl_df.tbl_df")) {
      s3_register("vctrs::vec_cast", "tbl_df.tbl_df", vec_cast_tbl_df_tbl_df)
      s3_register(
        "vctrs::vec_cast",
        "tbl_df.data.frame",
        vec_cast_tbl_df_data.frame
      )
      s3_register(
        "vctrs::vec_cast",
        "data.frame.tbl_df",
        vec_cast_data.frame_tbl_df
      )
    }
  })

  on_package_load("dplyr", {
    if (!env_has(ns_env("dplyr"), "vec_restore.grouped_df")) {
      s3_register("vctrs::vec_restore", "grouped_df", vec_restore_grouped_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_ptype2.grouped_df.grouped_df")) {
      s3_register(
        "vctrs::vec_ptype2",
        "grouped_df.grouped_df",
        vec_ptype2_grouped_df_grouped_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "grouped_df.data.frame",
        vec_ptype2_grouped_df_data.frame
      )
      s3_register(
        "vctrs::vec_ptype2",
        "grouped_df.tbl_df",
        vec_ptype2_grouped_df_tbl_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "data.frame.grouped_df",
        vec_ptype2_data.frame_grouped_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "tbl_df.grouped_df",
        vec_ptype2_tbl_df_grouped_df
      )
    }

    if (!env_has(ns_env("dplyr"), "vec_cast.grouped_df.grouped_df")) {
      s3_register(
        "vctrs::vec_cast",
        "grouped_df.grouped_df",
        vec_cast_grouped_df_grouped_df
      )
      s3_register(
        "vctrs::vec_cast",
        "grouped_df.data.frame",
        vec_cast_grouped_df_data.frame
      )
      s3_register(
        "vctrs::vec_cast",
        "grouped_df.tbl_df",
        vec_cast_grouped_df_tbl_df
      )
      s3_register(
        "vctrs::vec_cast",
        "data.frame.grouped_df",
        vec_cast_data.frame_grouped_df
      )
      s3_register(
        "vctrs::vec_cast",
        "tbl_df.grouped_df",
        vec_cast_tbl_df_grouped_df
      )
    }

    if (!env_has(ns_env("dplyr"), "vec_restore.rowwise_df")) {
      s3_register("vctrs::vec_restore", "rowwise_df", vec_restore_rowwise_df)
    }

    if (!env_has(ns_env("dplyr"), "vec_ptype2.rowwise_df.rowwise_df")) {
      s3_register(
        "vctrs::vec_ptype2",
        "rowwise_df.rowwise_df",
        vec_ptype2_rowwise_df_rowwise_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "rowwise_df.data.frame",
        vec_ptype2_rowwise_df_data.frame
      )
      s3_register(
        "vctrs::vec_ptype2",
        "rowwise_df.tbl_df",
        vec_ptype2_rowwise_df_tbl_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "data.frame.rowwise_df",
        vec_ptype2_data.frame_rowwise_df
      )
      s3_register(
        "vctrs::vec_ptype2",
        "tbl_df.rowwise_df",
        vec_ptype2_tbl_df_rowwise_df
      )
    }

    if (!env_has(ns_env("dplyr"), "vec_cast.rowwise_df.rowwise_df")) {
      s3_register(
        "vctrs::vec_cast",
        "rowwise_df.rowwise_df",
        vec_cast_rowwise_df_rowwise_df
      )
      s3_register(
        "vctrs::vec_cast",
        "rowwise_df.data.frame",
        vec_cast_rowwise_df_data.frame
      )
      s3_register(
        "vctrs::vec_cast",
        "rowwise_df.tbl_df",
        vec_cast_rowwise_df_tbl_df
      )
      s3_register(
        "vctrs::vec_cast",
        "data.frame.rowwise_df",
        vec_cast_data.frame_rowwise_df
      )
      s3_register(
        "vctrs::vec_cast",
        "tbl_df.rowwise_df",
        vec_cast_tbl_df_rowwise_df
      )
    }
  })

  on_package_load("sf", {
    if (!is_installed("sf (>= 1.0-25)")) {
      sf_update <- function(...) {
        abort(
          "Please update to sf >=1.0-25 for sf + vctrs/tidyverse compatibility."
        )
      }
      s3_register("vctrs::vec_proxy", "sf", sf_update)
      s3_register("vctrs::vec_restore", "sf", sf_update)
      s3_register("vctrs::vec_ptype2", "sf.sf", sf_update)
      s3_register("vctrs::vec_ptype2", "sf.data.frame", sf_update)
      s3_register("vctrs::vec_ptype2", "data.frame.sf", sf_update)
      s3_register("vctrs::vec_ptype2", "sf.tbl_df", sf_update)
      s3_register("vctrs::vec_ptype2", "tbl_df.sf", sf_update)
      s3_register("vctrs::vec_cast", "sf.sf", sf_update)
      s3_register("vctrs::vec_cast", "sf.data.frame", sf_update)
      s3_register("vctrs::vec_cast", "data.frame.sf", sf_update)
      s3_register("vctrs::vec_proxy_order", "sfc", sf_update)
    }
  })

  on_package_load("data.table", {
    if (!env_has(ns_env("data.table"), "vec_proxy.IDate")) {
      s3_register("vctrs::vec_proxy", "IDate", vec_proxy_IDate)
      s3_register("vctrs::vec_restore", "IDate", vec_restore_IDate)
    }
  })

  .Call(vctrs_init_library, ns_env())
}

# nocov end
