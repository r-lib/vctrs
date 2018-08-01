context("test-list_of")

test_that("list_of works like list", {
  x1 <- list_of(1, 1)
  expect_type(x1, "list")
  expect_s3_class(x1, "list_of")
  expect_equal(attr(x1, "type"), double())

  x2 <- list_of(1, 1, .type = integer())
  expect_equal(attr(x1, "type"), integer())

  x3 <- as_list_of(list(1, 1))
  expect_equal(x3, x1)
})

test_that("list_of errors if it can't find common type", {
  expect_error(list_of(1, "a"), class = "error_no_max_type")
  expect_error(list_of(), "find common type")
})

test_that("can use as_list_of to change type", {
  x1 <- list_of(1)
  expect_equal(as_list_of(x1), x1)

  x2 <- as_list_of(x1, .type = integer())
  expect_identical(x2[[1]], 1L)
})

test_that("print method gives human friendly output", {
  x <- list_of(1, 2:3)

  expect_known_output({
      print(x)
      cat("\n")
      print(tibble::tibble(x))
    },
    file = test_path("test-list_of-print.txt")
  )
})

# Subsetting --------------------------------------------------------------

test_that("[ preserves type", {
  x <- list_of(1)
  expect_equal(x[1], x)
})

test_that("[<-, [[<- and $<- coerce their input", {
  x <- list_of(x = 1, y = 1, z = 1)
  x[1] <- list(FALSE)
  x[[2]] <- FALSE
  x$z <- FALSE

  expect_equal(x, list_of(x = 0, y = 0, z = 0))
})
