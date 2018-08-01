context("test-repeated")

test_that("repeated works like list", {
  x1 <- repeated(1, 1)
  expect_type(x1, "list")
  expect_s3_class(x1, "repeated")
  expect_equal(attr(x1, "type"), double())

  x2 <- repeated(1, 1, .type = integer())
  expect_equal(attr(x1, "type"), integer())

  x3 <- as_repeated(list(1, 1))
  expect_equal(x3, x1)
})

test_that("repeated errors if it can't find common type", {
  expect_error(repeated(1, "a"), class = "error_no_max_type")
  expect_error(repeated(), "find common type")
})

test_that("can use as_repeated to change type", {
  x1 <- repeated(1)
  expect_equal(as_repeated(x1), x1)

  x2 <- as_repeated(x1, .type = integer())
  expect_identical(x2[[1]], 1L)
})

test_that("print method gives human friendly output", {
  x <- repeated(1, 2:3)

  expect_known_output({
      print(x)
      cat("\n")
      print(tibble::tibble(x))
    },
    file = test_path("test-repeated-print.txt")
  )
})

# Subsetting --------------------------------------------------------------

test_that("[ preserves type", {
  x <- repeated(1)
  expect_equal(x[1], x)
})

test_that("[<-, [[<- and $<- coerce their input", {
  x <- repeated(x = 1, y = 1, z = 1)
  x[1] <- list(FALSE)
  x[[2]] <- FALSE
  x$z <- FALSE

  expect_equal(x, repeated(x = 0, y = 0, z = 0))
})
