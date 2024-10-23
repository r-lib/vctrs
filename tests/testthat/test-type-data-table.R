# Never run on CRAN, even if they have data.table, because we don't regularly
# check these on CI and we don't want a change in data.table to force a CRAN
# failure for vctrs.
skip_on_cran()

# Avoids adding `data.table` to Suggests.
# These tests are only run on the devs' machines.
testthat_import_from("data.table", "data.table")

test_that("common type of data.table and data.frame is data.table", {
  expect_identical(
    vec_ptype2(data.table(x = TRUE), data.table(y = 2)),
    data.table(x = lgl(), y = dbl())
  )
  expect_identical(
    vec_ptype2(data.table(x = TRUE), data.frame(y = 2)),
    data.table(x = lgl(), y = dbl())
  )
  expect_identical(
    vec_ptype2(data.frame(y = 2), data.table(x = TRUE)),
    data.table(y = dbl(), x = lgl())
  )

  expect_identical(
    vec_cast(data.table(y = 2), data.table(x = TRUE, y = 1L)),
    data.table(x = NA, y = 2L)
  )
  expect_identical(
    vec_cast(data.frame(y = 2), data.table(x = TRUE, y = 1L)),
    data.table(x = NA, y = 2L)
  )
  expect_identical(
    vec_cast(data.table(y = 2), data.frame(x = TRUE, y = 1L)),
    data.frame(x = NA, y = 2L)
  )
})

test_that("data.table and tibble do not have a common type", {
  expect_equal(
    vec_ptype_common(data.table(x = TRUE), tibble(y = 2)),
    tibble(x = lgl(), y = dbl())
  )
  expect_equal(
    vec_ptype_common(tibble(y = 2), data.table(x = TRUE)),
    tibble(y = dbl(), x = lgl())
  )

  expect_identical(
    vec_cast(data.table(y = 2), tibble(x = TRUE, y = 1L)),
    tibble(x = lgl(NA), y = 2L)
  )
  expect_identical(
    vec_cast(tibble(y = 2), data.table(x = TRUE, y = 1L)),
    data_frame(x = lgl(NA), y = 2L)
  )
})

test_that("data table has formatting methods", {
  expect_snapshot({
    dt <- data.table(x = 1, y = 2, z = 3)
    vec_ptype_abbr(dt)
    vec_ptype_full(dt)
  })
})
