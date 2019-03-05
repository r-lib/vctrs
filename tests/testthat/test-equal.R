context("test-equal")


# vectorised --------------------------------------------------------------

test_that("throws error for unsuported type", {
  expect_error(.Call(vctrs_equal, expression(x), expression(x), TRUE), "Unsupported")
})

test_that("C wrapper throws error if length or type doesn't match", {
  expect_error(.Call(vctrs_equal, 1:2, 1L, TRUE), "same types and lengths")
  expect_error(.Call(vctrs_equal, 1, 1L, TRUE), "same types and lengths")
})

test_that("correct behaviour for basic vectors", {
  expect_equal(vec_equal(c(TRUE, FALSE), TRUE), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1L, 2L), 1L), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1, 2), 1), c(TRUE, FALSE))
  expect_equal(vec_equal(c("1", "2"), "1"), c(TRUE, FALSE))
  expect_equal(vec_equal(list(1:3, 1:2), list(1:3)), c(TRUE, FALSE))
  expect_equal(vec_equal(list(1:3, 1.5), list(1:3)), c(TRUE, FALSE))
})

test_that("NAs are equal", {
  expect_true(vec_equal(NA, NA, na_equal = TRUE))
  expect_true(vec_equal(NA_integer_, NA_integer_, na_equal = TRUE))
  expect_true(vec_equal(NA_real_, NA_real_, na_equal = TRUE))
  expect_true(vec_equal(NA_character_, NA_character_, na_equal = TRUE))
})

test_that("double special values", {
  expect_equal(vec_equal(c(NaN, NA), NaN, na_equal = TRUE), c(TRUE, FALSE))
  expect_equal(vec_equal(c(NA, NaN), NA, na_equal = TRUE), c(TRUE, FALSE))
  expect_true(vec_equal(Inf, Inf))
  expect_true(vec_equal(-Inf, -Inf))
})

test_that("can compare data frames", {
  df <- data.frame(x = 1:2, y = letters[2:1], stringsAsFactors = FALSE)
  expect_equal(vec_equal(df, df[1, ]), c(TRUE, FALSE))
})

test_that("data frames must have same size and columns", {
  expect_false(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(x = 1, y = 2),
    TRUE
  ))

  expect_false(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(y = 1),
    TRUE
  ))

  expect_false(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(x = 2),
    TRUE
  ))

  expect_false(.Call(vctrs_equal,
    list(data.frame(x = 1)),
    list(10),
    TRUE
  ))

})

# object ------------------------------------------------------------------

test_that("can compare NULL",{
  expect_true(obj_equal(NULL, NULL))
})

test_that("can compare objects with reference semantics", {
  expect_true(obj_equal(globalenv(), globalenv()))
  expect_false(obj_equal(globalenv(), environment()))

  expect_true(obj_equal(quote(x), quote(x)))
  expect_false(obj_equal(quote(x), quote(y)))
})

test_that("can compare pairlists", {
  expect_true(obj_equal(quote(x + y), quote(x + y)))
  expect_true(obj_equal(pairlist(x = 1, y = 2), pairlist(x = 1, y = 2)))
})

test_that("can compare functions", {
  f1 <- function(x, y) x + y
  f2 <- function(x, y) x + y
  expect_false(obj_equal(f2, f1))

  attr(f1, "srcref") <- NULL
  attr(f2, "srcref") <- NULL
  expect_true(obj_equal(f2, f1))

  f3 <- f1
  formals(f3) <- alist(x = 1)
  expect_false(obj_equal(f3, f1))

  f4 <- f1
  body(f4) <- quote(x)
  expect_false(obj_equal(f4, f2))
})

test_that("not equal if different types or lengths", {
  expect_false(obj_equal(1, 2))
  expect_false(obj_equal(1:2, 1))
})

test_that("not equal if attributes not equal", {
  x1 <- structure(1:10, x = 1, y = 2)
  x2 <- structure(1:10, x = 1, y = 3)
  expect_false(obj_equal(x1, x2))
})

# na ----------------------------------------------------------------------

test_that("can detect different types of NA", {
  expect_true(vec_equal_na(NA))
  expect_true(vec_equal_na(NA_integer_))
  expect_true(vec_equal_na(NA_real_))
  expect_true(vec_equal_na(NaN))
  expect_true(vec_equal_na(NA_character_))
  expect_true(vec_equal_na(list(NULL)))
})

test_that("vectorised over rows of a data frame", {
  df <- data.frame(x = c(1, 1, NA, NA), y = c(1, NA, 1, NA))
  expect_equal(vec_equal_na(df), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("NA propagate symmetrically (#204)", {
  exp <- c(NA, NA)

  expect_identical(vec_equal(c(TRUE, FALSE), NA), exp)
  expect_identical(vec_equal(1:2, NA), exp)
  expect_identical(vec_equal(c(1, 2), NA), exp)
  expect_identical(vec_equal(letters[1:2], NA), exp)

  expect_identical(vec_equal(NA, c(TRUE, FALSE)), exp)
  expect_identical(vec_equal(NA, 1:2), exp)
  expect_identical(vec_equal(NA, c(1, 2)), exp)
  expect_identical(vec_equal(NA, letters[1:2]), exp)
})

test_that("NA propagate from data frames columns", {
  x <- data.frame(x = 1:3)
  y <- data.frame(x = c(1L, NA, 2L))

  expect_identical(vec_equal(x, y), c(TRUE, NA, FALSE))
  expect_identical(vec_equal(y, x), c(TRUE, NA, FALSE))

  expect_identical(vec_equal(x, y, na_equal = TRUE), c(TRUE, FALSE, FALSE))
  expect_identical(vec_equal(y, x, na_equal = TRUE), c(TRUE, FALSE, FALSE))

  x <- data.frame(x = 1:3, y = 1:3)
  y <- data.frame(x = c(1L, NA, 2L), y = c(NA, 2L, 3L))

  expect_identical(vec_equal(x, y), c(NA, NA, FALSE))
  expect_identical(vec_equal(y, x), c(NA, NA, FALSE))

  expect_identical(vec_equal(x, y, na_equal = TRUE), c(FALSE, FALSE, FALSE))
  expect_identical(vec_equal(y, x, na_equal = TRUE), c(FALSE, FALSE, FALSE))
})

test_that("NA propagate from list components", {
  expect_identical(obj_equal(NA, NA, na_equal = FALSE), NA)
  expect_identical(vec_equal(list(NA), list(NA)), NA)

  expect_true(obj_equal(NA, NA, na_equal = TRUE))
  expect_true(vec_equal(list(NA), list(NA), na_equal = TRUE))
})

test_that("NA propagate from vector names when comparing objects (#217)", {
  x <- set_names(1:3, c("a", "b", NA))
  y <- set_names(1:3, c("a", NA, NA))

  expect_identical(obj_equal(x, x, na_equal = FALSE), NA)
  expect_identical(obj_equal(x, x, na_equal = TRUE), TRUE)

  expect_identical(obj_equal(x, y, na_equal = FALSE), NA)
  expect_identical(obj_equal(x, y, na_equal = TRUE), FALSE)

  expect_identical(vec_equal(list(x, x, y), list(x, y, y)), c(NA, NA, NA))
  expect_identical(vec_equal(list(x, x, y), list(x, y, y), na_equal = TRUE), c(TRUE, FALSE, TRUE))
})


# proxy -------------------------------------------------------------------

test_that("compound objects create data frames", {
  df <- data.frame(x = 1:2, y = 2:1)
  expect_s3_class(vec_proxy_equal(df), "data.frame")

  posixlt <- as.POSIXlt(as.Date("2010-10-10") + 0:5)
  expect_s3_class(vec_proxy_equal(posixlt), "data.frame")
})

