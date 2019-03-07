context("kind")

test_that("bare atomic vectors are atomic", {
  expect_identical(vec_kind(TRUE), "atomic")
  expect_identical(vec_kind(1L), "atomic")
  expect_identical(vec_kind(1), "atomic")
  expect_identical(vec_kind(1i), "atomic")
  expect_identical(vec_kind("foo"), "atomic")
  expect_identical(vec_kind(as.raw(1)), "atomic")
})

test_that("S3 atomic vectors are atomic by default", {
  expect_identical(vec_kind(foobar(TRUE)), "atomic")
  expect_identical(vec_kind(foobar(1L)), "atomic")
  expect_identical(vec_kind(foobar(1)), "atomic")
  expect_identical(vec_kind(foobar(1i)), "atomic")
  expect_identical(vec_kind(foobar("foo")), "atomic")
  expect_identical(vec_kind(foobar(as.raw(1))), "atomic")
})

test_that("bare lists are recursive", {
  expect_identical(vec_kind(list()), "recursive")
})

test_that("S3 lists are atoms by default", {
  expect_identical(vec_kind(structure(list(), class = "foobar")), "atom")
})

test_that("data frames are atomic and recursive by default", {
  expect_identical(vec_kind(mtcars), c("atomic", "recursive"))
})

test_that("vec_kind() checks returned kind", {
  vec_kind.vctrs_foobar <- function(x) c("atom", "recursive")
  expect_error(vec_kind(foobar()), "Unexpected vector kind combination `atom` and `recursive`")

  vec_kind.vctrs_foobar <- function(x) c("atomium")
  expect_error(vec_kind(foobar()), "Unexpected vector kind `atomium`")
})
