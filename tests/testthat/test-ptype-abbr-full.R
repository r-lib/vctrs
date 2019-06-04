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
  expect_equal(vec_ptype_abbr(ones(10, 0)), "dbl[,0]")

  expect_equal(vec_ptype_full(ones(10)), "double")
  expect_equal(vec_ptype_full(ones(0, 10)), "double[,10]")
  expect_equal(vec_ptype_full(ones(10, 0)), "double[,0]")

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

test_that("complex and factor as expected (#323)", {
  expect_equal(vec_ptype_abbr(0i), "cpl")
  expect_equal(vec_ptype_abbr(factor()), "fct")
})

test_that("I() wraps contents", {
  f <- factor()

  expect_equal(vec_ptype_abbr(I(f)), "I<fct>")
  expect_equal(vec_ptype_full(I(f)), "I<factor<>>")
})

test_that("AsIs class stripped from I()", {
  df <- data.frame(x = 1, y = 1:2)
  class(df) <- c("myclass", "data.frame")

  expect_equal(vec_ptype_full(I(df)), "I<myclass<\n  x: double\n  y: integer\n>>")
  expect_equal(vec_ptype_full(I(df[1])), "I<myclass<x:double>>")
  expect_equal(vec_ptype_full(I(df[0])), "I<myclass<>>")
})

test_that("named lists are tagged (#322)", {
  expect_identical(vec_ptype_abbr(list(x = 1, y = 2)), "named list")
})
