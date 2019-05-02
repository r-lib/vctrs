context("test-type-sclr")

test_that("sclr is a named list", {
  x <- new_sclr(x = 1, y = 2)
  expect_type(x, "list")
  expect_s3_class(x, "vctrs_sclr")
  expect_named(x, c("x", "y"))
})

test_that("scalar must have unique names", {
  expect_error(new_sclr(x = 1, x = 2), "not TRUE")
})

test_that("can get and set existing fields", {
  x <- new_sclr(x = 1, y = 2)

  x$x <- 3
  expect_equal(x$x, 3)
  x[["y"]] <- 4
  expect_equal(x[["y"]], 4)

  expect_error(x$z, "Invalid index")
  expect_error(x$z <- 1, "Invalid index")
})

test_that("as.list strips attributes apart from names", {
  x <- new_sclr(x = 1, y = 2)
  y <- as.list(x)

  expect_type(y, "list")
  expect_equal(attributes(y), list(names = names(x)))
})

test_that("as.data.frame works", {
  # #167: Not sure if this is the correct behavior.
  x <- new_sclr(x = 1, y = 2)
  expect_equal(
    as.data.frame(x, nm = "a"),
    new_data_frame(list(a = list(x)))
  )
})

test_that("putting in a data frame makes a list-col", {
  x <- new_sclr(x = 1, y = 2)
  df <- data.frame(x)

  expect_s3_class(df, "data.frame")
  expect_equal(df$x, list(x))
})

test_that("vector operations are unsupported", {
  x <- new_sclr(x = 1, y = 2)

  expect_error(x["a"], class = "vctrs_error_unsupported")
  expect_error(x["a"] <- 1, class = "vctrs_error_unsupported")

  expect_error(names(x) <- "x", class = "vctrs_error_unsupported")
  expect_error(dim(x) <- 1, class = "vctrs_error_unsupported")
  expect_error(dimnames(x) <- 1, class = "vctrs_error_unsupported")
  expect_error(levels(x) <- 1, class = "vctrs_error_unsupported")
  expect_error(is.na(x) <- 1, class = "vctrs_error_unsupported")

  expect_error(c(x), class = "vctrs_error_unsupported")
  expect_error(abs(x), class = "vctrs_error_unsupported")
  expect_error(x + 1, class = "vctrs_error_unsupported")
  expect_error(Arg(x), class = "vctrs_error_unsupported")
  expect_error(sum(x), class = "vctrs_error_unsupported")
  expect_error(order(x), class = "vctrs_error_unsupported")
  expect_error(levels(x), class = "vctrs_error_unsupported")
  expect_error(t(x), class = "vctrs_error_unsupported")
  expect_error(unique(x), class = "vctrs_error_unsupported")
  expect_error(duplicated(x), class = "vctrs_error_unsupported")
  expect_error(anyDuplicated(x), class = "vctrs_error_unsupported")
  expect_error(as.logical(x), class = "vctrs_error_unsupported")
  expect_error(as.integer(x), class = "vctrs_error_unsupported")
  expect_error(as.double(x), class = "vctrs_error_unsupported")
  expect_error(as.character(x), class = "vctrs_error_unsupported")
  expect_error(as.Date(x), class = "vctrs_error_unsupported")
  expect_error(as.POSIXct(x), class = "vctrs_error_unsupported")
})

test_that("summary is unimplemented", {
  x <- new_sclr(x = 1, y = 2)
  expect_error(summary(x), class = "vctrs_error_unimplemented")
})
