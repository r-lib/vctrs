context("test-type-tibble")


test_that("tibble beats data frame", {
  df <- vec_ptype(data_frame())
  dt <- vec_ptype(tibble::tibble())

  expect_s3_class(vec_ptype(dt, df)[[1]], "tbl_df")
  expect_s3_class(vec_ptype(dt, df)[[1]], "tbl_df")
})

test_that("recast makes tibbles", {
  df1 <- tibble::tibble(x = 1:4)
  df2 <- vec_restore(vec_data(df1), df1)

  expect_s3_class(df1, "tbl_df")
})

