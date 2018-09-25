context("test-type-tibble")

test_that("tibble beats data frame", {
  df <- vec_ptype(new_data_frame())
  dt <- vec_ptype(tibble::tibble())

  expect_s3_class(vec_ptype(dt, df)[[1]], "tbl_df")
  expect_s3_class(vec_ptype(df, dt)[[1]], "tbl_df")
})

test_that("can cast tibble to df and vice versa", {
  df <- new_data_frame()
  dt <- tibble::tibble()

  expect_equal(vec_cast(df, dt), dt)
  expect_equal(vec_cast(dt, df), df)
})

test_that("vec_restore restores tibbles", {
  df1 <- tibble::tibble(x = 1:4)
  df2 <- vec_restore(vec_data(df1), df1)

  expect_s3_class(df2, "tbl_df")
})
