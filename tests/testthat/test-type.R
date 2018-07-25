context("test-type")

test_that("atomic vectors and arrays as expected", {
  expect_equal(vec_type(1:5), "integer")

  dbl_mat <- matrix(1, nrow = 3, ncol = 3)
  expect_equal(vec_type(dbl_mat), "double[,3]")
})

test_that("date/times as expected", {
  expect_equal(vec_type(Sys.Date()), "date")
  expect_equal(vec_type(Sys.time()), "datetime")
})

test_that("factors display hashed levels", {
  f1 <- factor(letters)

  expect_equal(vec_type(f1), "factor<5cab7>")
  expect_equal(vec_type(f1[0]), "factor<5cab7>")
})
