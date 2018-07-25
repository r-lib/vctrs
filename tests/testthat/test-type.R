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

test_that("data frames print nicely", {
  expect_known_output(
    file = test_path("test-type-df.txt"),
    {
      cat("mtcars:\n")
      print(vec_type(mtcars))
      cat("\n")
      cat("iris:\n")
      print(vec_type(iris))
    }
  )
})

test_that("embedded data frames", {
  df <- data.frame(x = 1:3)
  df$y <- data.frame(a = 1:3, b = letters[1:3])

  expect_known_output(
    file = test_path("test-type-df-embedded.txt"),
    {
      print(vec_type(df))
    }
  )
})
