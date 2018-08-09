context("test-list_of")

test_that("list_of works like list", {
  x1 <- list_of(1, 1)
  expect_type(x1, "list")
  expect_s3_class(x1, "list_of")
  expect_equal(attr(x1, "ptype"), double())

  x2 <- list_of(1, 1, .ptype = integer())
  expect_equal(attr(x1, "ptype"), integer())

  x3 <- as_list_of(list(1, 1))
  expect_equal(x3, x1)
})

test_that("list_of errors if it can't find common type", {
  expect_error(list_of(1, "a"), class = "error_incompatible_type")
  expect_error(list_of(), "find common type")
})

test_that("can use as_list_of to change type", {
  x1 <- list_of(1)
  expect_equal(as_list_of(x1), x1)

  x2 <- as_list_of(x1, .ptype = integer())
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
  # Seems to be some bug in R 3.1 where NextMethod() called from $.list_of
  # causes an error "invalid subscript type 'promise'"
  skip_if_not(getRversion() >= "3.2")

  x <- list_of(x = 1, y = 1, z = 1)
  x[1] <- list(FALSE)
  x[[2]] <- FALSE
  x$z <- FALSE

  expect_equal(x, list_of(x = 0, y = 0, z = 0))
})


# Type system -------------------------------------------------------------

test_that("max<list_of<a>, list_of<b>> is list_of<max<a, b>>", {
  r_int <- list_of(.ptype = integer())
  r_dbl <- list_of(.ptype = double())

  expect_equal(vec_ptype(r_int, r_int), vec_ptype(r_int))
  expect_equal(vec_ptype(r_int, r_dbl), vec_ptype(r_int))
})

test_that("safe casts work as expected", {
  x <- list_of(1)
  expect_equal(vec_cast(NULL, x), NULL)
  expect_equal(vec_cast(list(1), x), x)
  expect_equal(vec_cast(list(TRUE), x), x)
})

test_that("lossy casts generate warning", {
  x <- list_of(1L)
  expect_condition(vec_cast(list(1.5), x), class = "warning_cast_lossy")
  expect_condition(vec_cast(list_of(1L), list()), class = "warning_cast_lossy")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), list_of(1)), class = "error_incompatible_cast")
})
