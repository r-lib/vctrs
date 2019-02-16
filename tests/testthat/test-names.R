context("test-names")

# vec_slice --------------------------------------------------------------

c_na <- function(...) {
  x <- c(...)
  names(x)[names(x) == ""] <- NA_character_
  x
}

test_that("can use names to vec_slice() a named object", {
  x0 <- c(a = 1, b = 2)
  x1 <- c(a = 1, a = 2)

  expect_identical(vec_slice(x0, letters[1]), c(a = 1))
  expect_identical(vec_slice(x0, letters[2:1]), c(b = 2, a = 1))
  expect_identical(vec_slice(x0, letters[3:1]), c_na(NA_real_, b = 2, a = 1))
  expect_identical(vec_slice(x1, letters[1]), c(a = 1))
  expect_identical(vec_slice(x1, letters[2]), c_na(NA_real_))
})


test_that("can use names to vec_slice<-() a named object", {
  x0 <- c(a = 1, b = 2)
  x1 <- c(a = 1, a = 2)

  vec_slice(x0, "b") <- 3
  expect_identical(x0, c(a = 1, b = 3))

  vec_slice(x0, "c") <- 2
  expect_identical(x0, c(a = 1, b = 3, c = 2))

  vec_slice(x1, "a") <- 3
  expect_identical(x1, c(a = 3, a = 2))
})

test_that("can't use names to vec_slice() an unnamed object", {
  x0 <- 1:3

  expect_error(
    vec_slice(x0, letters[1]),
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]),
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
})


test_that("can use names to vec_slice<-() a named object", {
  x0 <- 1:3
  x1 <- NULL

  expect_error(
    vec_slice(x0, letters[1]) <- 4L,
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]) <- 5L,
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x1, letters[1]) <- 6,
    "Can't use character to index an unnamed vector.",
    fixed = TRUE
  )
})
