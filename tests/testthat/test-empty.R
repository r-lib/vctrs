test_that("can compact missing elements", {
  x <- list(NULL, 1, NULL)
  expect_identical(list_drop_empty(x), list(1))
})

test_that("can compact empty elements", {
  x <- list(1, NULL, integer(), NULL)
  expect_identical(list_drop_empty(x), list(1))
})

test_that("emptyness works with data frames", {
  x <- data_frame()
  y <- data_frame(.size = 2L)

  lst <- list(x, y)

  expect_identical(list_drop_empty(lst), list(y))
})

test_that("emptyness works with rcrd types", {
  x <- new_rcrd(list(foo = integer(), bar = numeric()))
  y <- new_rcrd(list(foo = 1L, bar = 1))

  lst <- list(x, y)

  expect_identical(list_drop_empty(lst), list(y))
})

test_that("works with empty lists", {
  expect_identical(list_drop_empty(list()), list())
})

test_that("retains list type", {
  x <- list_of(NULL, integer())
  expect_identical(list_drop_empty(x), list_of(.ptype = integer()))
})

test_that("validates `x`", {
  expect_error(list_drop_empty(1), "must be a list")
  expect_error(list_drop_empty(data_frame()), "must be a list")
})
