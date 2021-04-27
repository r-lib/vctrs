
test_that("ptype2 base methods are not inherited", {
  ptypes <- vec_remove(base_empty_types, c("null", "dataframe"))
  for (ptype in ptypes) {
    x <- new_vctr(ptype, class = "foobar", inherit_base_type = TRUE)
    expect_s3_class(vec_ptype2(x, x), "foobar")
    expect_error(vec_ptype2(x, ptype), class = "vctrs_error_incompatible_type")
    expect_error(vec_ptype2(ptype, x), class = "vctrs_error_incompatible_type")
  }
})

test_that("cast base methods are not inherited", {
  ptypes <- vec_remove(base_empty_types, c("null", "dataframe"))
  for (ptype in ptypes) {
    x <- new_vctr(ptype, class = "foobar", inherit_base_type = TRUE)
    expect_s3_class(vec_cast(ptype, x), "foobar")
    expect_error(vec_cast(x, ptype), class = "vctrs_error_incompatible_type")
  }
})

test_that("default cast allows objects with the same type", {
  x <- structure(1, class = c("foo", "double"))
  expect_equal(vec_cast(x, x), x)
})

# vec_shaped_ptype -------------------------------------------------------

test_that("array dimensions are preserved", {
  mat1 <- matrix(lgl(), nrow = 1, ncol = 1)
  mat2 <- matrix(lgl(), nrow = 2, ncol = 2)
  mat3 <- matrix(lgl(), nrow = 2, ncol = 3)

  expect_equal(vec_ptype2(mat1, mat1), matrix(lgl(), nrow = 0, ncol = 1))
  expect_equal(vec_ptype2(mat1, mat2), matrix(lgl(), nrow = 0, ncol = 2))
  expect_error(vec_ptype2(mat2, mat3), class = "vctrs_error_incompatible_type")
})

test_that("vec_shaped_ptype()", {
  int <- function(...) array(NA_integer_, c(...))

  expect_identical(vec_shaped_ptype(integer(), int(5), int(10)), new_shape(integer()))
  expect_identical(vec_shaped_ptype(integer(), int(5, 1), int(10, 1)), new_shape(integer(), 1))
  expect_identical(vec_shaped_ptype(integer(), int(5, 1, 2), int(10, 1, 2)), new_shape(integer(), 1:2))
})

# vec_cast() --------------------------------------------------------------

# NULL

test_that("NULL is idempotent", {
  expect_equal(vec_cast(NULL, NULL), NULL)
  expect_equal(vec_cast(list(1:3), NULL), list(1:3))
})


# Logical

test_that("safe casts work as expected", {
  exp <- lgl(TRUE, FALSE)
  expect_equal(vec_cast(NULL, logical()), NULL)
  expect_equal(vec_cast(lgl(TRUE, FALSE), logical()), exp)
  expect_equal(vec_cast(int(1L, 0L), logical()), exp)
  expect_equal(vec_cast(dbl(1, 0), logical()), exp)

  # These used to be allowed
  expect_error(vec_cast(chr("T", "F"), logical()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(chr("TRUE", "FALSE"), logical()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(chr("true", "false"), logical()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(1, 0), logical()), class = "vctrs_error_incompatible_type")
})

test_that("NA casts work as expected", {
  exp <- lgl(NA)
  to <- lgl()

  expect_equal(vec_cast(lgl(NA), to), exp)
  expect_equal(vec_cast(int(NA), to), exp)
  expect_equal(vec_cast(dbl(NA), to), exp)

  # These used to be allowed
  expect_error(vec_cast(chr(NA), to), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(NA), to), class = "vctrs_error_incompatible_type")
})

test_that("Shaped NA casts work as expected", {
  mat <- matrix
  exp_mat <- mat(lgl(NA))
  to_mat <- matrix(lgl())

  expect_equal(vec_cast(mat(lgl(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(int(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(dbl(NA)), to_mat), exp_mat)

  # These used to be allowed
  expect_error(vec_cast(mat(chr(NA)), to_mat), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(mat(list(NA)), to_mat), class = "vctrs_error_incompatible_type")
})

test_that("lossy casts generate warning", {
  expect_lossy(vec_cast(int(2L, 1L), lgl()), lgl(TRUE, TRUE), x = int(),  to = lgl())
  expect_lossy(vec_cast(dbl(2, 1), lgl()), lgl(TRUE, TRUE), x = dbl(),  to = lgl())

  # These used to be allowed
  expect_error(vec_cast(chr("x", "TRUE"), lgl()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(chr("t", "T"), lgl()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(chr("f", "F"), lgl()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(c(TRUE, FALSE), TRUE), lgl()), class = "vctrs_error_incompatible_type")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), logical()), class = "vctrs_error_incompatible_type")
})

test_that("dimensionality matches output" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))

  x <- matrix(1, nrow = 2, ncol = 2)
  expect_error(vec_cast(x, logical()), class = "vctrs_error_incompatible_type")
})

test_that("the common type of two `NA` vectors is unspecified", {
  expect_equal(vec_ptype2(NA, NA), unspecified())
})


# Integer

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, integer()), NULL)
  expect_equal(vec_cast(lgl(TRUE, FALSE), integer()), int(1L, 0L))
  expect_equal(vec_cast(int(1L, 2L), integer()), int(1L, 2L))
  expect_equal(vec_cast(dbl(1, 2), integer()), int(1L, 2L))

  # These used to be allowed
  expect_error(vec_cast(chr("1", "2"), integer()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(1L, 2L), integer()), class = "vctrs_error_incompatible_type")
})

test_that("NA casts work as expected", {
  exp <- int(NA)
  to <- int()

  expect_equal(vec_cast(lgl(NA), to), exp)
  expect_equal(vec_cast(int(NA), to), exp)
  expect_equal(vec_cast(dbl(NA), to), exp)

  # These used to be allowed
  expect_error(vec_cast(chr(NA), to), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(NA), to), class = "vctrs_error_incompatible_type")
})

test_that("Shaped NA casts work as expected", {
  mat <- matrix
  exp_mat <- mat(int(NA))
  to_mat <- matrix(int())

  expect_equal(vec_cast(mat(lgl(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(int(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(dbl(NA)), to_mat), exp_mat)

  # These used to be allowed
  expect_error(vec_cast(mat(chr(NA)), to_mat), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(mat(list(NA)), to_mat), class = "vctrs_error_incompatible_type")
})

test_that("lossy casts generate error", {
  expect_lossy(vec_cast(c(2.5, 2), int()),     int(2, 2), x = dbl(), to = int())
  expect_lossy(vec_cast(c(.Machine$integer.max + 1, 1), int()),  int(NA, 1L), x = dbl(), to = int())
  expect_lossy(vec_cast(c(-.Machine$integer.max - 1, 1), int()), int(NA, 1L), x = dbl(), to = int())

  # These used to be allowed
  expect_error(vec_cast(c("2.5", "2"), int()), class = "vctrs_error_incompatible_type")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), integer()), class = "vctrs_error_incompatible_type")
})


# Double

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, double()), NULL)
  expect_equal(vec_cast(lgl(TRUE, FALSE), double()), dbl(1, 0))
  expect_equal(vec_cast(int(1, 0), double()), dbl(1, 0))
  expect_equal(vec_cast(dbl(1, 1.5), double()), dbl(1, 1.5))

  # These used to be allowed
  expect_error(vec_cast(chr("1", "1.5"), double()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(1, 1.5), double()), class = "vctrs_error_incompatible_type")
})

test_that("NA casts work as expected", {
  exp <- dbl(NA)
  to <- dbl()

  expect_equal(vec_cast(lgl(NA), to), exp)
  expect_equal(vec_cast(int(NA), to), exp)
  expect_equal(vec_cast(dbl(NA), to), exp)

  # These used to be allowed
  expect_error(vec_cast(chr(NA), to), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(NA), to), class = "vctrs_error_incompatible_type")
})

test_that("Shaped NA casts work as expected", {
  mat <- matrix
  exp_mat <- mat(dbl(NA))
  to_mat <- matrix(dbl())

  expect_equal(vec_cast(mat(lgl(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(int(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(dbl(NA)), to_mat), exp_mat)

  # These used to be allowed
  expect_error(vec_cast(mat(chr(NA)), to_mat), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(mat(list(NA)), to_mat), class = "vctrs_error_incompatible_type")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(factor("a"), double()), class = "vctrs_error_incompatible_type")
})


# Complex

test_that("safe casts to complex works", {
  expect_identical(vec_cast(NULL, cpl()), NULL)
  expect_identical(vec_cast(lgl(TRUE, FALSE), cpl()), cpl(1, 0))
  expect_identical(vec_cast(int(1, 0), cpl()), cpl(1, 0))
  expect_identical(vec_cast(dbl(1, 1.5), cpl()), cpl(1, 1.5))

  # This used to be allowed
  expect_error(vec_cast(list(1, 1.5), cpl()), class = "vctrs_error_incompatible_type")
})

test_that("NA casts work as expected", {
  exp <- cpl(NA)
  to <- cpl()

  expect_equal(vec_cast(lgl(NA), to), exp)
  expect_equal(vec_cast(int(NA), to), exp)
  expect_equal(vec_cast(dbl(NA), to), exp)

  # This used to be allowed
  expect_error(vec_cast(list(NA), to), class = "vctrs_error_incompatible_type")
})

test_that("Shaped NA casts work as expected", {
  mat <- matrix
  exp_mat <- mat(cpl(NA))
  to_mat <- matrix(cpl())

  expect_equal(vec_cast(mat(lgl(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(int(NA)), to_mat), exp_mat)
  expect_equal(vec_cast(mat(dbl(NA)), to_mat), exp_mat)

  # This used to be allowed
  expect_error(vec_cast(mat(list(NA)), to_mat), class = "vctrs_error_incompatible_type")
})

test_that("complex is coercible to numeric types", {
  expect_identical(vec_ptype2(cpl(), NULL), cpl())
  expect_identical(vec_ptype2(NULL, cpl()), cpl())

  expect_identical(vec_ptype2(cpl(), int()), cpl())
  expect_identical(vec_ptype2(int(), cpl()), cpl())

  expect_identical(vec_ptype2(cpl(), dbl()), cpl())
  expect_identical(vec_ptype2(dbl(), cpl()), cpl())

  expect_identical(vec_ptype2(cpl(), cpl()), cpl())

  expect_identical(vec_c(0, 1i), cpl(0i, 1i))
})

test_that("complex is not coercible to logical", {
  expect_error(vec_ptype2(cpl(), lgl()), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(lgl(), cpl()), class = "vctrs_error_incompatible_type")
})


# Character

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, character()), NULL)
  expect_equal(vec_cast(NA, character()), NA_character_)

  # These used to be allowed
  expect_error(vec_cast(lgl(TRUE, FALSE), character()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list("x", "y"), character()), class = "vctrs_error_incompatible_type")
})

test_that("NA casts work as expected", {
  exp <- chr(NA)
  to <- chr()

  expect_equal(vec_cast(lgl(NA), to), exp)
  expect_equal(vec_cast(chr(NA), to), exp)

  # These used to be allowed
  expect_error(vec_cast(int(NA), to), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(dbl(NA), to), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(list(NA), to), class = "vctrs_error_incompatible_type")
})

test_that("Shaped NA casts work as expected", {
  mat <- matrix
  exp_mat <- mat(chr(NA))
  to_mat <- matrix(chr())

  expect_equal(vec_cast(mat(chr(NA)), to_mat), exp_mat)

  # These used to be allowed
  expect_error(vec_cast(mat(lgl(NA)), to_mat), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(mat(int(NA)), to_mat), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(mat(dbl(NA)), to_mat), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(mat(list(NA)), to_mat), class = "vctrs_error_incompatible_type")
})

test_that("difftime does not get special treatment", {
  dt1 <- as.difftime(600, units = "secs")
  # This used to be allowed
  expect_error(vec_cast(dt1, character()), class = "vctrs_error_incompatible_type")
})


# Raw

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, raw()), NULL)

  # This used to be allowed
  expect_error(vec_cast(list(raw(1)), raw()), class = "vctrs_error_incompatible_type")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(raw(1), double()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(double(1), raw()), class = "vctrs_error_incompatible_type")
})

test_that("can sort raw", {
  x <- as.raw(c(3, 1, 2, 4))
  expect_identical(vec_order(x), int(2, 3, 1, 4))
  expect_identical(x[vec_order(x)], as.raw(1:4))
})

test_that("raw has informative type summaries", {
  expect_equal(vec_ptype_abbr(raw()), "raw")
  expect_equal(vec_ptype_full(raw()), "raw")
})

test_that("can provide common type with raw", {
  local_methods(
    vec_ptype2.raw.vctrs_foobar = function(...) "dispatched-left",
    vec_ptype2.vctrs_foobar = function(...) NULL,
    vec_ptype2.vctrs_foobar.raw = function(...) "dispatched-right"
  )
  expect_identical(vec_ptype2(raw(), foobar("")), "dispatched-left")
  expect_identical(vec_ptype2(foobar(""), raw()), "dispatched-right")
})


# Lists

test_that("safe casts work as expected", {
  expect_equal(vec_cast(NULL, list()), NULL)
  expect_equal(vec_cast(NA, list()), list(NULL))
  expect_equal(vec_cast(list(1L, 2L), list()), list(1L, 2L))

  # This used to be allowed
  expect_error(vec_cast(1:2, list()), class = "vctrs_error_incompatible_type")
})

test_that("dimensionality matches to" ,{
  x1 <- matrix(TRUE, nrow = 1, ncol = 1)
  x2 <- matrix(1L, nrow = 0, ncol = 2)
  expect_dim(vec_cast(x1, x2), c(1, 2))
  expect_dim(vec_cast(TRUE, x2), c(1, 2))
})

test_that("data frames are cast to list row wise (#639)", {
  x <- data.frame(x = 1:2, row.names = c("a", "b"))
  expect <- list(data.frame(x = 1L), data.frame(x = 2L))

  # This used to be allowed
  expect_error(vec_cast(x, list()), class = "vctrs_error_incompatible_type")
})

test_that("data frames can be cast to shaped lists", {
  to <- array(list(), dim = c(0, 2, 1))
  x <- data.frame(x = 1:2, y = 3:4)

  expect <- list(vec_slice(x, 1), vec_slice(x, 2))
  expect <- array(expect, dim = c(2, 2, 1))

  # This used to be allowed
  expect_error(vec_cast(x, to), class = "vctrs_error_incompatible_type")
})

test_that("Casting atomic `NA` values to list results in a `NULL`", {
  x <- c(NA, 1)
  expect <- list(NULL, 1)

  # This used to be allowed
  expect_error(vec_cast(x, list()), class = "vctrs_error_incompatible_type")
})

test_that("Casting data frame `NA` rows to list results in a `NULL`", {
  x <- data.frame(x = c(NA, NA, 1), y = c(NA, 1, 2))
  expect <- list(NULL, vec_slice(x, 2), vec_slice(x, 3))

  # This used to be allowed
  expect_error(vec_cast(x, list()), class = "vctrs_error_incompatible_type")
})


# Unspecified

test_that("unspecified can be cast to bare methods", {
  for (x in vectors[-4]) {
    expect_identical(vec_cast(unspecified(3), x), vec_init(x, 3))
  }
})
