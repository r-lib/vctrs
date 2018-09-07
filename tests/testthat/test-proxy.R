context("test-proxy")

# vec_proxy_equality ------------------------------------------------------

test_that("compound objects create data frames", {
  df <- data.frame(x = 1:2, y = 2:1)
  expect_s3_class(vec_proxy_equality(df), "data.frame")

  posixlt <- as.POSIXlt(as.Date("2010-10-10") + 0:5)
  expect_s3_class(vec_proxy_equality(posixlt), "data.frame")
})

