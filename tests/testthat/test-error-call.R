test_that("failing common type reports correct error call", {
  my_function <- function() vec_ptype2(2, chr())
  expect_snapshot((expect_error(my_function())))
})

test_that("failing cast reports correct error call", {
  my_function <- function() vec_cast(2, chr())
  expect_snapshot((expect_error(my_function())))
})

test_that("lossy cast reports correct error call", {
  my_function <- function() vec_cast(2, lgl())
  expect_snapshot((expect_error(my_function())))
})

test_that("failing common size reports correct error call", {
  my_function <- function() vec_recycle(1:2, 10)
  expect_snapshot((expect_error(my_function())))

  my_function <- function() vec_size_common(1:2, 1:10)
  expect_snapshot((expect_error(my_function())))
})

test_that("unsupported error reports correct error call", {
  x <- new_vctr(1:2)

  my_function <- function() dim(x) <- 1:2
  expect_snapshot((expect_error(my_function())))

  my_function <- function() median(x)
  expect_snapshot((expect_error(my_function())))
})

test_that("scalar error reports correct error call", {
  my_function <- function() vec_assert(foobar())
  expect_snapshot((expect_error(my_function())))
})
