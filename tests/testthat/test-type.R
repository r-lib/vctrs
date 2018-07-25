context("test-type")

test_that("atomic vectors and arrays as expected", {
  expect_equal(vec_type(1:5), vt("integer"))

  dbl_mat <- matrix(1, nrow = 3, ncol = 3)
  expect_equal(vec_type(dbl_mat), vt("double[,3]"))
})

test_that("date/times as expected", {
  expect_equal(vec_type(Sys.Date()), vt("date"))
  expect_equal(vec_type(Sys.time()), vt("datetime"))
})

test_that("factors display hashed levels", {
  f1 <- factor(letters)

  expect_equal(vec_type(f1), vt("factor<5cab7>"))
  expect_equal(vec_type(f1[0]), vt("factor<5cab7>"))
})
