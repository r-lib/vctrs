testthat_import_from_sf <- function(env = caller_env()) {
  # Avoids adding `sf` to Suggests.
  # These tests are only run on the devs' machines.
  testthat_import_from(
    "sf",
    c(
      "st_sf",
      "st_sfc",
      "st_point",
      "st_bbox",
      "st_precision",
      "st_crs",
      "st_linestring",
      "st_as_sf",
      "st_multipoint"
    ),
    env = env
  )
}
