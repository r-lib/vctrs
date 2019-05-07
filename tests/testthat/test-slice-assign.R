context("test-slice-assign")

test_that("slice-assign throws error with non-vector inputs", {
  x <- environment()
  expect_error(vec_slice(x, 1L) <- 1L, "a vector")
})

test_that("can modify subset", {
  x0 <- NULL
  vec_slice(x0, 1L) <- 1
  expect_identical(x0, NULL)

  x1 <- c(2, 1)
  vec_slice(x1, 1L) <- 1
  expect_equal(x1, c(1, 1))

  x2 <- array(c(2, 1, 2, 1), c(2, 2))
  vec_slice(x2, 1L) <- 1
  expect_equal(x2, array(1, c(2, 2)))

  x3 <- array(c(2, 1, 2, 1, 2, 1, 2, 1), c(2, 2, 2))
  vec_slice(x3, 1L) <- 1
  expect_equal(x3, array(1, c(2, 2, 2)))
})

test_that("can modify subset using logical index", {
  x1 <- c(2, 1)
  vec_slice(x1, TRUE) <- 3
  expect_equal(x1, c(3, 3))
  vec_slice(x1, c(TRUE, FALSE)) <- 4
  expect_equal(x1, c(4, 3))

  expect_error(
    vec_slice(x1, c(TRUE, FALSE, TRUE)) <- 5,
    "has size 3 whereas the index has size 2",
    fixed = TRUE
  )

  expect_error(
    vec_slice(mtcars, c(TRUE, FALSE)) <- mtcars[1, ],
    "has size 2 whereas the index has size 32"
  )
})

test_that("slice-assign ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)
  expect_equal(`vec_slice<-`(x, x > 0, 1), c(NA, 1, 1))
  expect_equal(`vec_slice<-`(x, x > 0, c(NA, 2:1)), c(NA, 2, 1))
})

test_that("slice-assign ignores NA in integer subsetting", {
  x <- 0:2
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), 1), c(0, 1, 1))
  expect_equal(`vec_slice<-`(x, c(NA, 2:3), c(NA, 2:1)), c(0, 2, 1))
})

test_that("can't modify subset with missing argument", {
  x <- 1:3
  expect_error(vec_slice(x, ) <- 2L)
})

test_that("can modify subset with recycled NA argument", {
  x <- 1:3
  vec_slice(x, NA) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with recycled TRUE argument", {
  x <- 1:3
  vec_slice(x, TRUE) <- 2L
  expect_identical(x, rep(2L, 3))
})

test_that("can modify subset with recycled FALSE argument", {
  x <- 1:3
  vec_slice(x, FALSE) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with NULL argument", {
  x <- 1:3
  vec_slice(x, NULL) <- 2L

  expect_identical(x, 1:3)
})

test_that("can slice-assign with missing indices", {
  x <- 1:3
  y <- 4:6
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect_identical(x, int(1, 5, 3))
})

test_that("slice-assign checks vectorness", {
  x <- foobar(list(1))
  expect_error(vec_slice(x, 1) <- 10, "must be a vector")
})

test_that("a coercible RHS is cast to LHS before assignment (#140)", {
  x <- 1:2
  expect_error(vec_slice(x, 1) <- "1", class = "vctrs_error_incompatible_type")

  x <- c("foo", "bar")
  expect_error(vec_slice(x, 1) <- 1, class = "vctrs_error_incompatible_type")

  x <- 1:2
  expect_error(vec_slice(x, 1) <- 3.5, class = "vctrs_error_cast_lossy")

  allow_lossy_cast(vec_slice(x, 1) <- 3.5)
  expect_identical(x, int(3, 2))
})
