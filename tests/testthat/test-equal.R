context("test-equal")


# vectorised --------------------------------------------------------------

test_that("throws error for unsuported type", {
  expect_error(.Call(vctrs_equal, expression(x), expression(x), TRUE), class = "vctrs_error_scalar_type")
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
  expect_equal(vec_equal(list(as.raw(1:3), as.raw(1.5)), list(as.raw(1:3))), c(TRUE, FALSE))
  expect_equal(vec_equal(c(1, 2) + 1i, 1+1i), c(TRUE, FALSE))
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
  expect_error(.Call(vctrs_equal,
    data.frame(x = 1),
    data.frame(x = 1, y = 2),
    TRUE
    ),
    "must have the same number of columns"
  )

  # Names are not checked, as `vec_cast_common()` should take care of the type.
  # So if `vec_cast_common()` is not called, or is improperly specified, then
  # this could result in false equality.
  expect_true(.Call(vctrs_equal,
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

test_that("can determine equality of strings with different encodings (#553)", {
  for (x_encoding in encodings()) {
    for (y_encoding in encodings()) {
      expect_equal(vec_equal(x_encoding, y_encoding), TRUE)
      expect_equal(vec_equal(x_encoding, y_encoding), x_encoding == y_encoding)
    }
  }
})

test_that("equality can be determined when strings have identical encodings", {
  encs <- c(encodings(), list(bytes = encoding_bytes()))

  for (enc in encs) {
    expect_true(vec_equal(enc, enc))
    expect_equal(vec_equal(enc, enc), enc == enc)
  }
})

test_that("equality is known to fail when comparing bytes to other encodings", {
  error <- "translating strings with \"bytes\" encoding"

  for (enc in encodings()) {
    expect_error(vec_equal(encoding_bytes(), enc), error)
    expect_error(vec_equal(enc, encoding_bytes()), error)
  }
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

test_that("works recursively with data frame columns", {
  df <- data.frame(x = c(1, 1, NA, NA))
  df$df <- data.frame(y = c(NA, 1, 1, NA), z = c(1, NA, 1, NA))
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
  # FIXME: Not clear what should we do in the recursive case. Should we
  # compare attributes of non S3 vectors at all?

  x <- set_names(1:3, c("a", "b", NA))
  y <- set_names(1:3, c("a", NA, NA))

  expect_identical(obj_equal(x, x, na_equal = FALSE), NA)
  expect_identical(obj_equal(x, x, na_equal = TRUE), TRUE)

  expect_identical(obj_equal(x, y, na_equal = FALSE), NA)
  expect_identical(obj_equal(x, y, na_equal = TRUE), FALSE)

  expect_identical(vec_equal(list(x, x, y), list(x, y, y)), c(NA, NA, NA))
  expect_identical(vec_equal(list(x, x, y), list(x, y, y), na_equal = TRUE), c(TRUE, FALSE, TRUE))
})

test_that("NA do not propagate from attributes", {
  x <- structure(1:3, foo = NA)
  y <- structure(1:3, foo = "")
  expect_true(obj_equal(x, x))
  expect_false(obj_equal(x, y))
})

test_that("NA do not propagate from function bodies or formals", {
  fn <- other <- function() NA
  body(other) <- TRUE

  expect_true(vec_equal(list(fn), list(fn)))
  expect_false(vec_equal(list(fn), list(other)))
  expect_true(obj_equal(fn, fn))
  expect_false(obj_equal(fn, other))

  fn <- other <- function(x = NA) NULL
  formals(other) <- list(x = NULL)

  expect_true(vec_equal(list(fn), list(fn)))
  expect_false(vec_equal(list(fn), list(other)))
})


# proxy -------------------------------------------------------------------

test_that("vec_equal() takes vec_proxy() by default", {
  scoped_env_proxy()
  x <- new_proxy(1:3)
  y <- new_proxy(3:1)
  expect_identical(vec_equal(x, y), lgl(FALSE, TRUE, FALSE))
})

test_that("vec_equal() takes vec_proxy_equal() if implemented", {
  scoped_comparable_tuple()

  x <- tuple(1:3, 1:3)
  y <- tuple(1:3, 4:6)

  expect_identical(x == y, rep(TRUE, 3))
  expect_identical(vec_equal(x, y), rep(TRUE, 3))

  # Recursive case
  foo <- data_frame(x = x)
  bar <- data_frame(x = y)
  expect_identical(vec_equal(foo, bar), rep(TRUE, 3))
})

# duplicate all ----------------------------------------------------------

test_that("throws error for unsupported type", {
  expect_error(vec_duplicate_all(expression(x)), class = "vctrs_error_scalar_type")
})

test_that("correct behaviour for basic vectors", {
  expect_true(vec_duplicate_all(c(TRUE, TRUE)))
  expect_true(vec_duplicate_all(c(2L, 2L)))
  expect_true(vec_duplicate_all(c(2, 2)))
  expect_true(vec_duplicate_all(c("1", "1")))
  expect_true(vec_duplicate_all(list(1:3, 1:3)))
  expect_true(vec_duplicate_all(as.raw(c(1, 1))))
  expect_true(vec_duplicate_all(c(1, 1) + 2i))

  expect_false(vec_duplicate_all(c(TRUE, FALSE)))
  expect_false(vec_duplicate_all(c(1L, 2L)))
  expect_false(vec_duplicate_all(c(1, 2)))
  expect_false(vec_duplicate_all(c("1", "2")))
  expect_false(vec_duplicate_all(list(1, 1:3)))
  expect_false(vec_duplicate_all(as.raw(c(1, 2))))
  expect_false(vec_duplicate_all(c(1, 2) + 2i))
})

test_that("correct behavior for size 0 input", {
  expect_true(vec_duplicate_all(NULL))
  expect_true(vec_duplicate_all(integer()))
  expect_true(vec_duplicate_all(list()))
  expect_true(vec_duplicate_all(new_data_frame()))
})

test_that("correct behavior for size 1 input", {
  expect_true(vec_duplicate_all(1))
  expect_true(vec_duplicate_all(list(1)))
  expect_true(vec_duplicate_all(data_frame(x = 1)))
})

test_that("NAs are considered duplicates", {
  expect_true(vec_duplicate_all(c(NA, NA)))
  expect_true(vec_duplicate_all(c(NA_integer_, NA_integer_)))
  expect_true(vec_duplicate_all(c(NA_real_, NA_real_)))
  expect_true(vec_duplicate_all(c(NA_character_, NA_character_)))
  expect_true(vec_duplicate_all(list(NULL, NULL)))
})

test_that("double special values", {
  expect_true(vec_duplicate_all(c(NaN, NaN)))
  expect_false(vec_duplicate_all(c(NaN, NA)))
  expect_false(vec_duplicate_all(c(NA, NaN)))

  expect_true(vec_duplicate_all(c(Inf, Inf)))
  expect_true(vec_duplicate_all(c(-Inf, -Inf)))
})

test_that("data frames are compared row wise", {
  df1 <- data_frame(x = c(1, 1), y = c("a", "a"))
  expect_true(vec_duplicate_all(df1))

  df2 <- data_frame(x = c(1, 2), y = c("a", "a"))
  expect_false(vec_duplicate_all(df2))
})

test_that("the equality proxy is taken recursively", {
  scoped_comparable_tuple()

  x <- tuple(c(1, 1, 2), 1:3)
  df <- data_frame(x = x)

  y <- tuple(c(1, 1, 1), 1:3)
  df2 <- data_frame(y = y)

  expect_equal(vec_duplicate_all(df), FALSE)
  expect_equal(vec_duplicate_all(df2), TRUE)
})

test_that("can detect duplicates among strings with different encodings", {
  for (x_encoding in encodings()) {
    for (y_encoding in encodings()) {
      expect_equal(vec_duplicate_all(c(x_encoding, y_encoding)), TRUE)
    }
  }
})

test_that("duplicate detection is known to fail when comparing bytes to other encodings", {
  error <- "translating strings with \"bytes\" encoding"

  for (enc in encodings()) {
    expect_error(vec_duplicate_all(c(encoding_bytes(), enc)), error)
  }
})
