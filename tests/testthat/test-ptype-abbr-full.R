context("test-type-string")

test_that("input must be a vector", {
  expect_error(vec_ptype_abbr(sum), "Not a vector")
  expect_error(vec_ptype_full(sum), "Not a vector")
})

test_that("NULL has method", {
  expect_equal(vec_ptype_abbr(NULL), "NULL")
  expect_equal(vec_ptype_full(NULL), "NULL")
})

test_that("non objects default to type + shape", {
  expect_equal(vec_ptype_abbr(ones(10)), "dbl")
  expect_equal(vec_ptype_abbr(ones(0, 10)), "dbl[,10]")
  expect_equal(vec_ptype_abbr(ones(10, 0)), "dbl[10,]")

  expect_equal(vec_ptype_full(ones(10)), "double")
  expect_equal(vec_ptype_full(ones(0, 10)), "double[,10]")
  expect_equal(vec_ptype_full(ones(10, 0)), "double[10,]")

})

test_that("objects default to first class", {
  x <- structure(1, class = "foofy")
  expect_equal(vec_ptype_full(x), "foofy")
  expect_equal(vec_ptype_abbr(x), "foofy")
})

test_that("atomic vectors and arrays as expected", {
  expect_equal(vec_ptype_full(1:5), "integer")

  dbl_mat <- array(double(), c(0, 3))
  expect_equal(vec_ptype_full(dbl_mat), "double[,3]")
})

test_that("I() wraps contents", {
  f <- factor()

  expect_equal(vec_ptype_abbr(I(f)), "I<fctr>")
  expect_equal(vec_ptype_full(I(f)), "I<factor<>>")
})

test_that("AsIs class stripped from I()", {
  df <- data.frame(x = 1, y = 1:2)
  class(df) <- c("myclass", "data.frame")

  expect_equal(vec_ptype_full(I(df)), "I<myclass<\n  x: double\n  y: integer\n>>")
  expect_equal(vec_ptype_full(I(df[1])), "I<myclass<x:double>>")
  expect_equal(vec_ptype_full(I(df[0])), "I<myclass<>>")
})

test_that("data frames print nicely", {
  expect_equal(vec_ptype_abbr(mtcars), "df[32,11]")

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
