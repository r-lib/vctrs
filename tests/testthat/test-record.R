context("test-record")


# constructor and accessors -----------------------------------------------

test_that("can construct and access components", {
  r <- new_record(list(x = 1, y = 2))

  expect_equal(length(r), 1)
  expect_equal(n_fields(r), 2)

  expect_equal(names(r), NULL)
  expect_equal(fields(r), c("x", "y"))

  expect_error(r$x, class = "error_unsupported")
  expect_equal(field(r, "x"), 1)
})

# invalid inputs ------------------------------------------------------------

test_that("must be list of equal length vectors", {
  expect_error(new_record(list()), "list of length 1")
  expect_error(new_record(list(x = environment())), "vector")
  expect_error(new_record(list(x = 1, y = 1:2)), "same length")
})

test_that("names must be unique", {
  expect_error(new_record(list(1, 2)), "unique names")
  expect_error(new_record(list(x = 1, 2)), "unique names")
  expect_error(new_record(list(x = 1, x = 2)), "unique names")
  expect_error(new_record(setNames(list(1, 2), "x")), "unique names")
})

test_that("no attributes", {
  x <- structure(list(x = 1:3), y = 1)
  expect_error(new_record(x), "no attributes")
})


# tuple class ----------------------------------------------------------
# use simple class to test essential features of records

test_that("print and str use format", {
  r <- tuple(1, 1:100)

  expect_known_output(
    file = test_path("test-record-format.txt"),
    {
      print(r)
      cat("\n")
      str(r[1:10])
      cat("\n")
      str(list(list(list(r, 1:100))))
    }
  )
})

test_that("subsetting methods applied to each field", {
  x <- tuple(1:2, 1)
  expect_equal(x[1], tuple(1, 1))
  expect_equal(x[[1]], tuple(1, 1))

  expect_equal(rep(tuple(1, 1), 2), tuple(c(1, 1), 1))

  length(x) <- 1
  expect_equal(x, tuple(1, 1))
})

test_that("subset assignment modifies each field", {
  x <- tuple(c(1, 1), c(2, 2))

  x[[1]] <- tuple(3, 3)
  expect_equal(x, tuple(c(3, 1), c(3, 2)))

  x[1] <- tuple(4, 4)
  expect_equal(x, tuple(c(4, 1), c(4, 2)))
})

test_that("subset assignment recycles", {
  x <- tuple(c(1, 1), c(2, 2))
  x[1:2] <- tuple(1, 1)
  expect_equal(x, tuple(c(1, 1), c(1, 1)))

  x[] <- tuple(2, 2)
  expect_equal(x, tuple(c(2, 2), c(2, 2)))
})
