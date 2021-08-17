test_that("can compact missing elements", {
  x <- list(1, NULL, integer(), NULL)
  expect_identical(list_compact(x, drop = "missing"), list(1, integer()))
})

test_that("can compact empty elements", {
  x <- list(1, NULL, integer(), NULL)
  expect_identical(list_compact(x, drop = "empty"), list(1))
})

test_that("emptyness works with data frames", {
  x <- data_frame()
  y <- data_frame(.size = 2L)

  lst <- list(x, y)

  expect_identical(list_compact(lst, drop = "empty"), list(y))
})

test_that("emptyness works with rcrd types", {
  x <- new_rcrd(list(foo = integer(), bar = numeric()))
  y <- new_rcrd(list(foo = 1L, bar = 1))

  lst <- list(x, y)

  expect_identical(list_compact(lst, drop = "empty"), list(y))
})

test_that("works with empty lists", {
  expect_identical(list_compact(list()), list())
})

test_that("retains list type", {
  x <- list_of(NULL, integer())
  expect_identical(list_compact(x), list_of(integer()))
  expect_identical(list_compact(x, drop = "empty"), list_of(.ptype = integer()))
})

test_that("validates `x`", {
  expect_error(list_compact(1), "must be a list")
  expect_error(list_compact(data_frame()), "must be a list")
})

test_that("validates `drop`", {
  expect_error(list_compact(1, drop = 1))
  expect_error(list_compact(1, drop = "miss"))
})
