context("test-type-string")

test_that("NULL as method", {
  expect_equal(vec_type_string(NULL), "NULL")
})

test_that("non objects default to type + shape", {
  expect_equal(vec_type_string(ones(10)), "double")
  expect_equal(vec_type_string(ones(0, 10)), "double[,10]")
  expect_equal(vec_type_string(ones(10, 0)), "double[10,]")
})

test_that("objects default to first class", {
  x <- structure(1, class = "foofy")
  expect_equal(vec_type_string(x), "foofy")
})

test_that("atomic vectors and arrays as expected", {
  expect_equal(vec_type_string(1:5), "integer")

  dbl_mat <- array(double(), c(0, 3))
  expect_equal(vec_type_string(dbl_mat), "double[,3]")
})

test_that("date/times as expected", {
  expect_equal(vec_type_string(Sys.Date()), "date")
  expect_equal(vec_type_string(Sys.time()), "datetime<local>")
})

test_that("difftime has units as parameter", {
  now <- Sys.time()

  expect_equal(vec_type_string(difftime(now + 10, now)), "difftime<secs>")
  expect_equal(vec_type_string(difftime(now + 1e5, now)), "difftime<days>")
})

test_that("data frames print nicely", {
  expect_known_output(
    file = test_path("test-type-df.txt"),
    {
      cat("mtcars:\n")
      print(vec_ptype(mtcars))
      cat("\n")
      cat("iris:\n")
      print(vec_ptype(iris))
    }
  )
})

test_that("embedded data frames print nicely", {
  df <- data.frame(x = 1:3)
  df$a <- data.frame(a = 1:3, b = letters[1:3])
  df$b <- list_of(1, 2, 3)
  df$c <- as_list_of(split(data.frame(x = 1:3, y = letters[1:3]), 1:3))


  expect_known_output(
    file = test_path("test-type-df-embedded.txt"),
    {
      print(vec_ptype(df))
    }
  )
})
