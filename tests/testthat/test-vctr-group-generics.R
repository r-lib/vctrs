context("test-vctr-group-generics")

test_that("default methods throw on incorrect type", {
  v <- new_vctr("x", class = "test")

  expect_error(vec_grp_unary("f", v), "mathematical operator")
  expect_error(vec_grp_numeric("f", v), "mathematical operator")
  expect_error(vec_grp_summary("f", v), "mathematical function")
  expect_error(vec_grp_logical("f", v), "Boolean operator")
})

test_that("default math functions preserve class", {
  v <- new_hidden(1)

  expect_s3_class(vec_grp_unary("+", v), "hidden")
  expect_s3_class(vec_grp_numeric("+", v, v), "hidden")
  expect_s3_class(vec_grp_summary("sum", v), "hidden")
})

test_that("default logical and comparison operations return bare logical", {
  v <- new_vctr(TRUE, class = "test")

  expect_identical(vec_grp_compare("==", v, v), TRUE)
  expect_identical(vec_grp_logical("&", v, v), TRUE)
  expect_identical(vec_grp_logical("!", v), FALSE)
})
