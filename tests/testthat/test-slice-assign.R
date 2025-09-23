test_that("slice-assign throws error with non-vector inputs", {
  x <- environment()
  expect_error(vec_slice(x, 1L) <- 1L, class = "vctrs_error_scalar_type")
})

test_that("slice-assign throws error with non-vector `value`", {
  x <- 1L
  expect_error(vec_slice(x, 1L) <- NULL, class = "vctrs_error_scalar_type")
  expect_error(
    vec_slice(x, 1L) <- environment(),
    class = "vctrs_error_scalar_type"
  )
})

test_that("assign throws error with non-vector `value`", {
  x <- 1L

  expect_snapshot(error = TRUE, {
    vec_assign(x, 1L, NULL)
  })
  expect_snapshot(error = TRUE, {
    vec_assign(x, 1L, NULL, slice_value = TRUE)
  })

  expect_snapshot(error = TRUE, {
    vec_assign(x, 1L, NULL, value_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_assign(x, 1L, NULL, slice_value = TRUE, value_arg = "foo")
  })

  expect_snapshot(error = TRUE, {
    vec_assign(x, 1L, environment(), value_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_assign(x, 1L, environment(), slice_value = TRUE, value_arg = "foo")
  })
})

test_that("can slice-assign NULL", {
  x <- NULL
  vec_slice(x, 1L) <- 1
  expect_identical(x, NULL)
})

test_that("can assign on NULL `x`", {
  x <- NULL
  expect_identical(vec_assign(x, TRUE, 1), NULL)
  expect_identical(vec_assign(x, TRUE, 1, slice_value = TRUE), NULL)
})

test_that("can slice-assign base vectors", {
  x <- rep(FALSE, 3)
  vec_slice(x, 2) <- TRUE
  expect_identical(x, lgl(FALSE, TRUE, FALSE))

  x <- rep(0L, 3)
  vec_slice(x, 2) <- 1L
  expect_identical(x, int(0L, 1L, 0L))

  x <- rep(0., 3)
  vec_slice(x, 2) <- 1
  expect_identical(x, dbl(0, 1, 0))

  x <- rep(0i, 3)
  vec_slice(x, 2) <- 1i
  expect_identical(x, cpl(0i, 1i, 0i))

  x <- rep("", 3)
  vec_slice(x, 2) <- "foo"
  expect_identical(x, chr("", "foo", ""))

  x <- as.raw(rep(0, 3))
  vec_slice(x, 2) <- as.raw(1)
  expect_identical(x, as.raw(c(0, 1, 0)))

  x <- rep(list(NULL), 3)
  vec_slice(x, 2) <- list(NA)
  expect_identical(x, list(NULL, NA, NULL))
})

test_that("can assign base vectors", {
  x <- rep(FALSE, 3)
  expect_identical(vec_assign(x, 2, TRUE), lgl(FALSE, TRUE, FALSE))
  expect_identical(x, rep(FALSE, 3))

  x <- rep(0L, 3)
  expect_identical(vec_assign(x, 2, 1L), int(0L, 1L, 0L))
  expect_identical(x, rep(0L, 3))

  x <- rep(0., 3)
  expect_identical(vec_assign(x, 2, 1), dbl(0, 1, 0))
  expect_identical(x, rep(0., 3))

  x <- rep(0i, 3)
  expect_identical(vec_assign(x, 2, 1i), cpl(0i, 1i, 0i))
  expect_identical(x, rep(0i, 3))

  x <- rep("", 3)
  expect_identical(vec_assign(x, 2, "foo"), chr("", "foo", ""))
  expect_identical(x, rep("", 3))

  x <- as.raw(rep(0, 3))
  expect_identical(vec_assign(x, 2, as.raw(1)), as.raw(c(0, 1, 0)))
  expect_identical(x, as.raw(rep(0, 3)))

  x <- rep(list(NULL), 3)
  expect_identical(vec_assign(x, 2, list(NA)), list(NULL, NA, NULL))
  expect_identical(x, rep(list(NULL), 3))
})

test_that("can assign base vectors with logical indices", {
  # Logical indices have their own optimized path so we test them specially

  condition <- c(TRUE, FALSE, NA, TRUE)

  x <- rep(FALSE, 4)
  value <- c(NA, TRUE, TRUE, TRUE)
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, lgl(NA, FALSE, FALSE, TRUE))

  x <- rep(0L, 4)
  value <- c(NA, 1L, 2L, 3L)
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, int(NA, 0L, 0L, 3L))

  x <- rep(0, 4)
  value <- c(NA, 1, 2, 3)
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, dbl(NA, 0, 0, 3))

  x <- rep(0i, 4)
  na <- complex(real = NA, imaginary = NA)
  value <- c(na, 1i, 2i, 3i)
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, c(na, 0i, 0i, 3i))

  x <- rep("", 4)
  value <- c(NA, "foo", "bar", "baz")
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, c(NA, "", "", "baz"))

  x <- as.raw(rep(0, 4))
  value <- as.raw(c(1, 2, 3, 4))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, as.raw(c(1, 0, 0, 4)))

  x <- rep(list(1), 4)
  value <- list(NA, 2, 3, 4)
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, list(NA, 1, 1, 4))
})

test_that("can assign base vectors with recycling of `value`", {
  x <- rep(FALSE, 3)
  expect_identical(vec_assign(x, c(3, 1), TRUE), lgl(TRUE, FALSE, TRUE))
  expect_identical(x, rep(FALSE, 3))

  x <- rep(0L, 3)
  expect_identical(vec_assign(x, c(3, 1), 1L), int(1L, 0L, 1L))
  expect_identical(x, rep(0L, 3))

  x <- rep(0., 3)
  expect_identical(vec_assign(x, c(3, 1), 1), dbl(1, 0, 1))
  expect_identical(x, rep(0., 3))

  x <- rep(0i, 3)
  expect_identical(vec_assign(x, c(3, 1), 1i), cpl(1i, 0i, 1i))
  expect_identical(x, rep(0i, 3))

  x <- rep("", 3)
  expect_identical(vec_assign(x, c(3, 1), "foo"), chr("foo", "", "foo"))
  expect_identical(x, rep("", 3))

  x <- as.raw(rep(0, 3))
  expect_identical(vec_assign(x, c(3, 1), as.raw(1)), as.raw(c(1, 0, 1)))
  expect_identical(x, as.raw(rep(0, 3)))

  x <- rep(list(NULL), 3)
  expect_identical(vec_assign(x, c(3, 1), list(NA)), list(NA, NULL, NA))
  expect_identical(x, rep(list(NULL), 3))
})

test_that("can assign base vectors with logical indices with recycled `value`", {
  # Logical indices have their own optimized path so we test them specially

  condition <- c(TRUE, FALSE, NA, TRUE)

  x <- rep(FALSE, 4)
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, lgl(NA, FALSE, FALSE, NA))

  x <- rep(0L, 4)
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, int(NA, 0L, 0L, NA))

  x <- rep(0, 4)
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, dbl(NA, 0, 0, NA))

  x <- rep(0i, 4)
  na <- complex(real = NA, imaginary = NA)
  value <- na
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, c(na, 0i, 0i, na))

  x <- rep("", 4)
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, c(NA, "", "", NA))

  x <- as.raw(rep(0, 4))
  value <- as.raw(1)
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, as.raw(c(1, 0, 0, 1)))

  x <- rep(list(1), 4)
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, list(NULL, 1, 1, NULL))
})

test_that("can assign shaped base vectors", {
  mat <- as.matrix

  x <- mat(rep(FALSE, 3))
  expect_identical(vec_assign(x, 2, TRUE), mat(lgl(FALSE, TRUE, FALSE)))
  expect_identical(x, mat(rep(FALSE, 3)))

  x <- mat(rep(0L, 3))
  expect_identical(vec_assign(x, 2, 1L), mat(int(0L, 1L, 0L)))
  expect_identical(x, mat(rep(0L, 3)))

  x <- mat(rep(0, 3))
  expect_identical(vec_assign(x, 2, 1), mat(dbl(0, 1, 0)))
  expect_identical(x, mat(rep(0, 3)))

  x <- mat(rep(0i, 3))
  expect_identical(vec_assign(x, 2, 1i), mat(cpl(0i, 1i, 0i)))
  expect_identical(x, mat(rep(0i, 3)))

  x <- mat(rep("", 3))
  expect_identical(vec_assign(x, 2, "foo"), mat(chr("", "foo", "")))
  expect_identical(x, mat(rep("", 3)))

  x <- mat(as.raw(rep(0, 3)))
  expect_identical(vec_assign(x, 2, as.raw(1)), mat(as.raw(c(0, 1, 0))))
  expect_identical(x, mat(as.raw(rep(0, 3))))

  mat <- as.matrix
  x <- mat(rep(list(NULL), 3))
  expect_identical(vec_assign(x, 2, list(NA)), mat(list(NULL, NA, NULL)))
  expect_identical(x, mat(rep(list(NULL), 3)))
})

test_that("can assign shaped base vectors with logical indices", {
  # Logical indices have their own optimized path so we test them specially

  mat <- as.matrix

  condition <- c(TRUE, FALSE, NA, TRUE)

  x <- mat(rep(FALSE, 4))
  value <- mat(c(NA, TRUE, TRUE, TRUE))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, FALSE, FALSE, TRUE)))

  x <- mat(rep(0L, 4))
  value <- mat(c(NA, 1L, 2L, 3L))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, 0L, 0L, 3L)))

  x <- mat(rep(0, 4))
  value <- mat(c(NA, 1, 2, 3))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, 0, 0, 3)))

  x <- mat(rep(0i, 4))
  value <- mat(c(1i, 2i, 3i, 4i))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(1i, 0i, 0i, 4i)))

  x <- mat(rep("", 4))
  value <- mat(c(NA, "foo", "bar", "baz"))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, "", "", "baz")))

  x <- mat(as.raw(rep(0, 4)))
  value <- mat(as.raw(c(1, 2, 3, 4)))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(as.raw(c(1, 0, 0, 4))))

  x <- mat(rep(list(1), 4))
  value <- mat(list(NA, 2, 3, 4))
  y <- vec_assign(x, condition, vec_slice(value, condition))
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(list(NA, 1, 1, 4)))
})

test_that("can assign shaped base vectors with recycling of `value`", {
  mat <- as.matrix

  x <- mat(rep(FALSE, 3))
  expect_identical(vec_assign(x, c(3, 1), TRUE), mat(lgl(TRUE, FALSE, TRUE)))
  expect_identical(x, mat(rep(FALSE, 3)))

  x <- mat(rep(0L, 3))
  expect_identical(vec_assign(x, c(3, 1), 1L), mat(int(1L, 0L, 1L)))
  expect_identical(x, mat(rep(0L, 3)))

  x <- mat(rep(0, 3))
  expect_identical(vec_assign(x, c(3, 1), 1), mat(dbl(1, 0, 1)))
  expect_identical(x, mat(rep(0, 3)))

  x <- mat(rep(0i, 3))
  expect_identical(vec_assign(x, c(3, 1), 1i), mat(cpl(1i, 0i, 1i)))
  expect_identical(x, mat(rep(0i, 3)))

  x <- mat(rep("", 3))
  expect_identical(vec_assign(x, c(3, 1), "foo"), mat(chr("foo", "", "foo")))
  expect_identical(x, mat(rep("", 3)))

  x <- mat(as.raw(rep(0, 3)))
  expect_identical(vec_assign(x, c(3, 1), as.raw(1)), mat(as.raw(c(1, 0, 1))))
  expect_identical(x, mat(as.raw(rep(0, 3))))

  mat <- as.matrix
  x <- mat(rep(list(NULL), 3))
  expect_identical(vec_assign(x, c(3, 1), list(NA)), mat(list(NA, NULL, NA)))
  expect_identical(x, mat(rep(list(NULL), 3)))
})

test_that("can assign shaped base vectors with logical indices with recycling of `value`", {
  # Logical indices have their own optimized path so we test them specially

  mat <- as.matrix

  condition <- c(TRUE, FALSE, NA, TRUE)

  x <- mat(rep(FALSE, 4))
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, FALSE, FALSE, NA)))

  x <- mat(rep(0L, 4))
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, 0L, 0L, NA)))

  x <- mat(rep(0, 4))
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, 0, 0, NA)))

  x <- mat(rep(0i, 4))
  value <- NA
  na <- complex(real = NA, imaginary = NA)
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(na, 0i, 0i, na)))

  x <- mat(rep("", 4))
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(c(NA, "", "", NA)))

  x <- mat(as.raw(rep(1, 4)))
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(as.raw(c(0, 1, 1, 0))))

  x <- mat(rep(list(1), 4))
  value <- NA
  y <- vec_assign(x, condition, value)
  z <- vec_assign(x, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, mat(list(NULL, 1, 1, NULL)))
})

test_that("can assign object of any dimensionality", {
  x1 <- ones(3)
  x2 <- ones(3, 4)
  x3 <- ones(3, 4, 5)
  x4 <- ones(3, 4, 5, 6)

  expect_identical(vec_assign(x1, 1L, 2L), array(rep(c(2, 1, 1), 1), dim = 3))
  expect_identical(
    vec_assign(x2, 1L, 2L),
    array(rep(c(2, 1, 1), 3), dim = c(3, 4))
  )
  expect_identical(
    vec_assign(x3, 1L, 2L),
    array(rep(c(2, 1, 1), 12), dim = c(3, 4, 5))
  )
  expect_identical(
    vec_assign(x4, 1L, 2L),
    array(rep(c(2, 1, 1), 60), dim = c(3, 4, 5, 6))
  )

  # With recycling of `value`
  expect_identical(
    vec_assign(x1, c(3L, 2L), 2L),
    array(rep(c(1, 2, 2), 1), dim = 3)
  )
  expect_identical(
    vec_assign(x2, c(3L, 2L), 2L),
    array(rep(c(1, 2, 2), 3), dim = c(3, 4))
  )
  expect_identical(
    vec_assign(x3, c(3L, 2L), 2L),
    array(rep(c(1, 2, 2), 12), dim = c(3, 4, 5))
  )
  expect_identical(
    vec_assign(x4, c(3L, 2L), 2L),
    array(rep(c(1, 2, 2), 60), dim = c(3, 4, 5, 6))
  )
})

test_that("can assign object of any dimensionality with logical indices", {
  # Logical indices have their own optimized path so we test them specially

  # Non barrier (integer, double, logical, etc)
  x1 <- ones(4)
  x2 <- ones(4, 5)
  x3 <- ones(4, 5, 6)
  x4 <- ones(4, 5, 6, 7)

  # Barrier (list, character)
  x1_list <- ones_list(4)
  x2_list <- ones_list(4, 5)
  x3_list <- ones_list(4, 5, 6)
  x4_list <- ones_list(4, 5, 6, 7)

  condition <- c(TRUE, FALSE, NA, TRUE)

  # No recycling
  value <- c(2, 3, 4, 5)
  value_list <- as.list(value)

  y <- vec_assign(x1, condition, vec_slice(value, condition))
  z <- vec_assign(x1, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 5), 1), dim = dim(x1)))

  y <- vec_assign(x1_list, condition, vec_slice(value_list, condition))
  z <- vec_assign(x1_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 5), 1), dim = dim(x1_list)))

  y <- vec_assign(x2, condition, vec_slice(value, condition))
  z <- vec_assign(x2, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 5), 4), dim = dim(x2)))

  y <- vec_assign(x2_list, condition, vec_slice(value_list, condition))
  z <- vec_assign(x2_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 5), 4), dim = dim(x2_list)))

  y <- vec_assign(x3, condition, vec_slice(value, condition))
  z <- vec_assign(x3, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 5), 20), dim = dim(x3)))

  y <- vec_assign(x3_list, condition, vec_slice(value_list, condition))
  z <- vec_assign(x3_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 5), 20), dim = dim(x3_list)))

  y <- vec_assign(x4, condition, vec_slice(value, condition))
  z <- vec_assign(x4, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 5), 120), dim = dim(x4)))

  y <- vec_assign(x4_list, condition, vec_slice(value_list, condition))
  z <- vec_assign(x4_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 5), 120), dim = dim(x4_list)))

  # `value` recycling
  value <- 2
  value_list <- as.list(value)

  y <- vec_assign(x1, condition, value)
  z <- vec_assign(x1, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 2), 1), dim = dim(x1)))

  y <- vec_assign(x1_list, condition, value_list)
  z <- vec_assign(x1_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 2), 1), dim = dim(x1_list)))

  y <- vec_assign(x2, condition, value)
  z <- vec_assign(x2, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 2), 4), dim = dim(x2)))

  y <- vec_assign(x2_list, condition, value_list)
  z <- vec_assign(x2_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 2), 4), dim = dim(x2_list)))

  y <- vec_assign(x3, condition, value)
  z <- vec_assign(x3, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 2), 20), dim = dim(x3)))

  y <- vec_assign(x3_list, condition, value_list)
  z <- vec_assign(x3_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 2), 20), dim = dim(x3_list)))

  y <- vec_assign(x4, condition, value)
  z <- vec_assign(x4, condition, value, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(c(2, 1, 1, 2), 120), dim = dim(x4)))

  y <- vec_assign(x4_list, condition, value_list)
  z <- vec_assign(x4_list, condition, value_list, slice_value = TRUE)
  expect_identical(y, z)
  expect_identical(y, array(rep(list(2, 1, 1, 2), 120), dim = dim(x4_list)))
})

test_that("atomics can't be assigned in lists", {
  x <- list(NULL)
  expect_error(vec_slice(x, 1) <- 1, class = "vctrs_error_incompatible_type")
  expect_error(vec_assign(x, 1, 2), class = "vctrs_error_incompatible_type")

  expect_error(
    vec_slice(x, 1) <- "foo",
    class = "vctrs_error_incompatible_type"
  )
  expect_error(vec_assign(x, 1, "foo"), class = "vctrs_error_incompatible_type")
})

test_that("Unspecified `NA` vector can be assigned into lists", {
  x <- list(1, 2)
  vec_slice(x, 1) <- NA
  expect_identical(x, list(NULL, 2))
})

test_that("monitoring test - unspecified() can be assigned in lists", {
  x <- list(1, 2)
  expect_error(vec_slice(x, 1) <- unspecified(1), NA)
  expect_equal(x, list(NULL, 2))
})

test_that("can assign and slice-assign data frames", {
  df <- data.frame(x = 1:2)
  df$y <- data.frame(a = 2:1)

  orig <- duplicate(df, shallow = FALSE)

  other <- data.frame(x = 3)
  other$y <- data.frame(a = 3)

  exp <- data.frame(x = int(3, 2))
  exp$y <- data.frame(a = int(3, 1))

  expect_identical(vec_assign(df, 1, other), exp)
  expect_identical(df, orig)

  expect_identical(vec_assign(df, 1, other, slice_value = TRUE), exp)
  expect_identical(df, orig)

  vec_slice(df, 1) <- other
  expect_identical(df, exp)
})

test_that("can assign using logical index", {
  x <- c(2, 1)
  vec_slice(x, TRUE) <- 3
  expect_equal(x, c(3, 3))

  vec_slice(x, c(TRUE, FALSE)) <- 4
  expect_equal(x, c(4, 3))

  expect_snapshot({
    (expect_error(
      vec_assign(x, c(TRUE, FALSE, TRUE), 5),
      class = "vctrs_error_subscript_size"
    ))
  })
  expect_snapshot({
    (expect_error(
      vec_assign(x, c(TRUE, FALSE, TRUE), 5, slice_value = TRUE),
      class = "vctrs_error_subscript_size"
    ))
  })

  expect_snapshot({
    (expect_error(
      vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ]),
      class = "vctrs_error_subscript_size"
    ))
  })
  expect_snapshot({
    (expect_error(
      vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ], slice_value = TRUE),
      class = "vctrs_error_subscript_size"
    ))
  })
})

test_that("assign `value` size depends on `slice_value`", {
  x <- c(1, 2, 3)

  # `value` size depends on number of `NA` or `TRUE` values in `i`
  expect_snapshot(error = TRUE, {
    vec_assign(x, c(TRUE, NA, FALSE), c(1, 2, 3))
  })

  # `value` size depends on size of `x`
  expect_snapshot(error = TRUE, {
    vec_assign(x, c(TRUE, NA, FALSE), c(1, 2), slice_value = TRUE)
  })
})

test_that("assign ignores NA in logical subsetting", {
  x <- c(NA, 1, 2)

  expect_equal(vec_assign(x, x > 0, 1), c(NA, 1, 1))
  expect_equal(vec_assign(x, x > 0, 1, slice_value = TRUE), c(NA, 1, 1))

  expect_equal(vec_assign(x, x > 0, c(NA, 2:1)), c(NA, 2, 1))
  expect_equal(
    vec_assign(x, x > 0, c(NA, 2:1), slice_value = TRUE),
    c(NA, 2, 1)
  )
})

test_that("assign with arrays ignores NA in logical subsetting", {
  mat <- as.matrix
  x <- c(NA, 1, 2)
  expect_equal(vec_assign(mat(x), x > 0, 1), mat(c(NA, 1, 1)))
  expect_equal(vec_assign(mat(x), x > 0, c(NA, 2:1)), mat(c(NA, 2, 1)))
})

test_that("assign ignores NA in integer subsetting", {
  x <- 0:2
  expect_equal(vec_assign(x, c(NA, 2:3), 1), c(0, 1, 1))
  expect_equal(vec_assign(x, c(NA, 2:3), c(NA, 2:1)), c(0, 2, 1))
})

test_that("assign with arrays ignores NA in integer subsetting", {
  mat <- as.matrix
  x <- mat(0:2)
  expect_equal(vec_assign(x, c(NA, 2:3), 1), mat(c(0, 1, 1)))
  expect_equal(vec_assign(x, c(NA, 2:3), c(NA, 2:1)), mat(c(0, 2, 1)))
})

test_that("can't modify subset with missing argument", {
  x <- 1:3
  expect_error(vec_slice(x, ) <- 2L)
})

test_that("can modify subset with recycled NA argument", {
  x <- 1:3
  vec_slice(x, NA) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with recycled TRUE argument", {
  x <- 1:3
  vec_slice(x, TRUE) <- 2L
  expect_identical(x, rep(2L, 3))
})

test_that("can modify subset with recycled FALSE argument", {
  x <- 1:3
  vec_slice(x, FALSE) <- 2L
  expect_identical(x, 1:3)
})

test_that("can modify subset with NULL argument", {
  x <- 1:3
  vec_slice(x, NULL) <- 2L
  expect_identical(x, 1:3)

  x <- vec_assign(x, NULL, 2L)
  expect_identical(x, 1:3)

  x <- vec_assign(x, NULL, 2L, slice_value = TRUE)
  expect_identical(x, 1:3)
})

test_that("can slice-assign with missing indices", {
  # Atomic case
  x <- 1:3
  y <- 4:6
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect_identical(x, int(1, 5, 3))

  # Barrier case
  x <- as.list(1:3)
  y <- as.list(4:6)
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect_identical(x, as.list(int(1, 5, 3)))

  # Atomic array case
  x <- array(1:12, dim = c(3, 2, 2))
  y <- array(13:24, dim = c(3, 2, 2))
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect <- array(
    int(1, 14, 3, 4, 17, 6, 7, 20, 9, 10, 23, 12),
    dim = c(3, 2, 2)
  )
  expect_identical(x, expect)

  # Barrier array case
  x <- array(as.list(1:12), dim = c(3, 2, 2))
  y <- array(as.list(13:24), dim = c(3, 2, 2))
  test <- c(NA, TRUE, FALSE)
  vec_slice(x, test) <- vec_slice(y, test)
  expect <- array(
    as.list(int(1, 14, 3, 4, 17, 6, 7, 20, 9, 10, 23, 12)),
    dim = c(3, 2, 2)
  )
  expect_identical(x, expect)
})

test_that("slice-assign checks vectorness", {
  x <- foobar(list(1))
  expect_error(vec_slice(x, 1) <- 10, class = "vctrs_error_scalar_type")
})

test_that("a coercible RHS is cast to LHS before assignment (#140)", {
  x <- 1:2
  expect_error(vec_slice(x, 1) <- "1", class = "vctrs_error_incompatible_type")

  x <- c("foo", "bar")
  expect_error(vec_slice(x, 1) <- 1, class = "vctrs_error_incompatible_type")

  x <- 1:2
  expect_error(vec_slice(x, 1) <- 3.5, class = "vctrs_error_cast_lossy")

  allow_lossy_cast(vec_slice(x, 1) <- 3.5)
  expect_identical(x, int(3, 2))

  x <- matrix(1:4, 2)
  vec_slice(x, 1) <- matrix(c(FALSE, FALSE), 1)
  expect_identical(x, matrix(int(0, 2, 0, 4), 2))
  expect_error(
    vec_assign(x, 1, matrix(c("", ""), 1)),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("slice-assign takes the proxy", {
  local_proxy()

  x <- new_proxy(1:3)
  y <- new_proxy(20:21)

  vec_slice(x, 2:3) <- y

  expect_identical(proxy_deref(x), int(1, 20, 21))
})

test_that("can use names to assign with a named object", {
  x0 <- c(a = 1, b = 2, c = 3)
  x1 <- c(a = 1, a = 2, a = 3)

  vec_slice(x0, "b") <- 4
  expect_identical(x0, c(a = 1, b = 4, c = 3))

  # Only first is changed
  vec_slice(x1, "a") <- 4
  expect_identical(x1, c(a = 4, a = 2, a = 3))

  x <- c(a = 1, b = 2, c = 3)

  expect_identical(
    vec_assign(x, c("c", "a"), c(4, 5)),
    c(a = 5, b = 2, c = 4)
  )
  # Slices `value` by `i` after matching `i` against `x` names
  expect_identical(
    vec_assign(x, c("c", "a"), c(4, 5, 6), slice_value = TRUE),
    c(a = 4, b = 2, c = 6)
  )

  expect_snapshot(error = TRUE, {
    vec_assign(x, c("c", "a"), c(4, 5, 6))
  })
  expect_snapshot(error = TRUE, {
    vec_assign(x, c("c", "a"), c(4, 5), slice_value = TRUE)
  })
})

test_that("can't use names to assign with an unnamed object", {
  x0 <- 1:3

  expect_error(
    vec_slice(x0, letters[1]) <- 4L,
    "Can't use character names to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_assign(x0, letters[1], 4L, slice_value = TRUE),
    "Can't use character names to index an unnamed vector.",
    fixed = TRUE
  )
  expect_error(
    vec_slice(x0, letters[25:27]) <- 5L,
    "Can't use character names to index an unnamed vector.",
    fixed = TRUE
  )
})

test_that("slice-assign falls back to `[<-` when proxy is not implemented", {
  obj <- foobar(c("foo", "bar", "baz"))
  expect_error(
    vec_slice(obj, 1:2) <- TRUE,
    class = "vctrs_error_incompatible_type"
  )

  vec_slice(obj, 1:2) <- foobar("quux")

  vec_ptype2(foobar(""), foobar(""))
  vec_cast(foobar(""), foobar(""))
  #> Error: Can't cast <vctrs_foobar> to <vctrs_foobar>

  local_methods(
    `[<-.vctrs_foobar` = function(x, i, value) {
      x <- unclass(x)
      x[i] <- "dispatched"
      x
    },
    vec_ptype2.logical.vctrs_foobar = function(...) foobar(""),
    vec_ptype2.vctrs_foobar = function(...) foobar(""),
    vec_cast.vctrs_foobar = function(...) NULL,
    vec_cast.vctrs_foobar.logical = function(x, to, ...) {
      foobar(rep("", length(x)))
    }
  )

  obj <- foobar(c("foo", "bar", "baz"))

  obj2 <- vec_assign(obj, 1:2, TRUE)
  expect_identical(obj2, foobar(c("dispatched", "dispatched", "baz")))

  obj2 <- vec_assign(obj, c(TRUE, FALSE, TRUE), TRUE)
  expect_identical(obj2, foobar(c("dispatched", "bar", "dispatched")))

  # We handle `NA` for the end user
  obj2 <- vec_assign(obj, c(1, NA, 2), TRUE)
  expect_identical(obj2, foobar(c("dispatched", "dispatched", "baz")))
})

test_that("vec_assign() can always assign unspecified values into foreign vector types", {
  obj <- foobar(c("foo", "bar", "baz"))
  expect <- foobar(c(NA, "bar", "baz"))

  expect_identical(vec_assign(obj, 1, NA), expect)
  expect_identical(vec_assign(obj, 1, unspecified(1)), expect)

  expect_identical(
    vec_assign(obj, 1, c(NA, NA, NA), slice_value = TRUE),
    expect
  )
  expect_identical(
    vec_assign(obj, 1, unspecified(3), slice_value = TRUE),
    expect
  )
})

test_that("slice-assign casts to `to` before falling back to `[<-` (#443)", {
  called <- FALSE

  local_methods(
    vec_proxy.vctrs_proxy = proxy_deref,
    vec_ptype2.vctrs_proxy = function(...) NULL,
    vec_ptype2.vctrs_proxy.vctrs_foobar = function(...) new_proxy(NA),
    vec_cast.vctrs_foobar = function(...) NULL,
    vec_cast.vctrs_foobar.vctrs_proxy = function(x, ...) foobar(proxy_deref(x)),
    `[<-.vctrs_foobar` = function(x, i, value) {
      called <<- TRUE
      expect_identical(value, foobar(10))
    }
  )

  x <- foobar(1)
  y <- new_proxy(10)
  vec_slice(x, 1) <- y
  expect_true(called)
})

test_that("index and value are sliced before falling back", {
  # Works around a bug in base R `[<-` before we call it, in particular, related
  # to the fact that `[<-` doesn't allow `NA` in subassign indices, but we do.

  lhs <- foobar(int(1:5))

  # With location vector
  expect_identical(
    vec_assign(lhs, c(NA, 1), foobar(int(6:7))),
    foobar(int(7, 2:5))
  )

  # With location vector and `slice_value = TRUE`
  expect_identical(
    vec_assign(lhs, c(NA, 1), foobar(int(6:10)), slice_value = TRUE),
    foobar(int(6, 2:5))
  )

  # With condition vector
  expect_identical(
    vec_assign(lhs, c(NA, FALSE, TRUE, NA, TRUE), foobar(int(6:9))),
    foobar(int(1, 2, 7, 4, 9))
  )

  # With condition vector and `slice_value = TRUE`
  expect_identical(
    vec_assign(
      lhs,
      c(NA, FALSE, TRUE, NA, TRUE),
      foobar(int(6:10)),
      slice_value = TRUE
    ),
    foobar(int(1, 2, 8, 4, 10))
  )
})

test_that("size 1 value is expected to be handled by the `[<-` fallback", {
  # i.e., we don't pre recycle size 1 value to the size of the index because
  # we expect that most `[<-` fallbacks eventually call base `[<-`, which
  # recycles size 1 value efficiently. See `vec_assign_fallback()`.
  lhs <- foobar(1:4)
  rhs <- foobar(0L)
  exp <- foobar(c(1L, 0L, 3L, 0L))

  expect_identical(vec_assign(lhs, c(2L, 4L), rhs), exp)

  expect_identical(
    vec_assign(lhs, c(FALSE, TRUE, FALSE, TRUE), rhs),
    exp
  )
  expect_identical(
    vec_assign(lhs, c(FALSE, TRUE, FALSE, TRUE), rhs, slice_value = TRUE),
    exp
  )
})

test_that("can assign to data frame", {
  x <- data_frame(x = 1:3)
  y <- data_frame(x = 20)
  expect_identical(vec_assign(x, 2, y), data_frame(x = int(1, 20, 3)))
})

test_that("can assign to data frame with `slice_value`", {
  x <- data_frame(x = 1:4)
  y <- data_frame(x = 21:24)
  i <- c(TRUE, FALSE, NA, TRUE)
  expect_identical(
    vec_assign(x, i, y, slice_value = TRUE),
    data_frame(x = int(21, 2, 3, 24))
  )
})

test_that("can assign to a data frame with matrix columns (#625)", {
  df <- tibble(x = 1:2, y = matrix(1:4, nrow = 2))
  expect_identical(vec_assign(df, 2L, df[1, ]), vec_slice(df, c(1, 1)))
})

test_that("assigning to a factor doesn't produce corrupt levels (#853)", {
  x <- factor(c("a", NA), levels = c("a", "b"))
  value <- factor("b", levels = "b")

  res <- vec_assign(x, 2, value)
  expect_identical(res, factor(c("a", "b")))

  res <- vec_assign(x, 1:2, value)
  expect_identical(res, factor(c("b", "b"), levels = c("a", "b")))
})

test_that("can slice-assign unspecified vectors with default type2 method", {
  local_rational_class()
  x <- rational(1:2, 2:3)
  x[[1]] <- NA
  expect_identical(x, rational(c(NA, 2L), c(NA, 3L)))
})

test_that("`vec_assign()` evaluates arg lazily", {
  expect_silent(vec_assign(1L, 1L, 1L, x_arg = print("oof")))
  expect_silent(vec_assign(1L, 1L, 1L, value_arg = print("oof")))
})

test_that("`vec_assign()` requires recyclable value", {
  expect_snapshot({
    (expect_error(
      vec_assign(1:3, 1:2, 1:3),
      class = "vctrs_error_recycle_incompatible_size"
    ))
  })
  expect_snapshot({
    (expect_error(
      vec_assign(1:3, 1:2, 1:2, slice_value = TRUE),
      class = "vctrs_error_recycle_incompatible_size"
    ))
  })
})

test_that("logical subscripts must match size of indexed vector", {
  expect_snapshot({
    (expect_error(
      vec_assign(1:2, c(TRUE, FALSE, TRUE), 5),
      class = "vctrs_error_subscript_size"
    ))
  })

  expect_snapshot(
    (expect_error(
      vec_assign(mtcars, c(TRUE, FALSE), mtcars[1, ]),
      class = "vctrs_error_subscript_size"
    ))
  )
})

test_that("must assign existing elements", {
  expect_snapshot({
    (expect_error(
      vec_assign(1:3, 5, 10),
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      vec_assign(1:3, "foo", 10),
      "unnamed vector"
    ))
    (expect_error(
      vec_slice(letters, -100) <- "foo",
      class = "vctrs_error_subscript_oob"
    ))
    (expect_error(
      vec_assign(set_names(letters), "foo", "bar"),
      class = "vctrs_error_subscript_oob"
    ))
  })
})

test_that("must assign with proper negative locations", {
  expect_snapshot({
    (expect_error(
      vec_assign(1:3, c(-1, 1), 1:2),
      class = "vctrs_error_subscript_type"
    ))
    (expect_error(
      vec_assign(1:3, c(-1, NA), 1:2),
      class = "vctrs_error_subscript_type"
    ))
  })
})

test_that("`vec_assign()` error args can be overridden", {
  expect_snapshot({
    (expect_error(
      vec_assign(1:2, 1L, "x", x_arg = "foo", value_arg = "bar"),
      class = "vctrs_error_incompatible_type"
    ))
    (expect_error(
      vec_assign(1:2, 1L, 1:2, value_arg = "bar"),
      class = "vctrs_error_recycle_incompatible_size"
    ))
  })
})

test_that("names are not assigned by default", {
  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L)
  vec_out <- c(a = 1L, b = 4L, c = 3L)
  expect_identical(
    vec_assign(vec_x, 2, vec_y),
    vec_out
  )

  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4L), row.names = "FOO")
  df_out <- new_data_frame(list(x = c(1L, 4L, 3L)), row.names = letters[1:3])
  expect_identical(
    vec_assign(df_x, 2, df_y),
    df_out
  )

  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4L, 1, dimnames = list("FOO"))
  mat_out <- matrix(c(1L, 4L, 3L), dimnames = list(letters[1:3]))
  expect_identical(
    vec_assign(mat_x, 2, mat_y),
    mat_out
  )

  nested_x <- new_data_frame(
    list(df = df_x, mat = mat_x, vec = vec_x),
    row.names = c("foo", "bar", "baz")
  )
  nested_y <- new_data_frame(
    list(df = df_y, mat = mat_y, vec = vec_y),
    row.names = c("quux")
  )
  nested_out <- new_data_frame(
    list(df = df_out, mat = mat_out, vec = vec_out),
    row.names = c("foo", "bar", "baz")
  )
  expect_identical(
    vec_assign(nested_x, 2, nested_y),
    nested_out
  )
})

test_that("can optionally assign names", {
  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L)
  vec_out <- c(a = 1L, FOO = 4L, c = 3L)
  expect_identical(
    vec_assign_params(vec_x, 2, vec_y, assign_names = TRUE),
    vec_out
  )

  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4L), row.names = "FOO")
  df_out <- new_data_frame(
    list(x = c(1L, 4L, 3L)),
    row.names = c("a", "FOO", "c")
  )
  expect_identical(
    vec_assign_params(df_x, 2, df_y, assign_names = TRUE),
    df_out
  )

  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4L, 1, dimnames = list("FOO"))
  mat_out <- matrix(c(1L, 4L, 3L), dimnames = list(c("a", "FOO", "c")))
  expect_identical(
    vec_assign_params(mat_x, 2, mat_y, assign_names = TRUE),
    mat_out
  )

  nested_x <- new_data_frame(
    list(df = df_x, mat = mat_x, vec = vec_x),
    row.names = c("foo", "bar", "baz")
  )
  nested_y <- new_data_frame(
    list(df = df_y, mat = mat_y, vec = vec_y),
    row.names = c("quux")
  )
  nested_out <- new_data_frame(
    list(df = df_out, mat = mat_out, vec = vec_out),
    row.names = c("foo", "quux", "baz")
  )

  expect_identical(
    vec_assign_params(nested_x, 2, nested_y, assign_names = TRUE),
    nested_out
  )
})

test_that("can optionally assign names with `slice_value`", {
  vec_x <- set_names(1:3, letters[1:3])
  vec_y <- c(FOO = 4L, BAR = 5L, BAZ = 6L)
  vec_out <- c(a = 1L, BAR = 5L, BAZ = 6L)
  expect_identical(
    vec_assign_params(
      vec_x,
      c(FALSE, TRUE, TRUE),
      vec_y,
      assign_names = TRUE,
      slice_value = TRUE
    ),
    vec_out
  )

  df_x <- new_data_frame(list(x = 1:3), row.names = letters[1:3])
  df_y <- new_data_frame(list(x = 4:6), row.names = c("FOO", "BAR", "BAZ"))
  df_out <- new_data_frame(
    list(x = c(1L, 5L, 6L)),
    row.names = c("a", "BAR", "BAZ")
  )
  expect_identical(
    vec_assign_params(
      df_x,
      c(FALSE, TRUE, TRUE),
      df_y,
      assign_names = TRUE,
      slice_value = TRUE
    ),
    df_out
  )

  mat_x <- matrix(1:3, 3, dimnames = list(letters[1:3]))
  mat_y <- matrix(4:6, 3, dimnames = list(c("FOO", "BAR", "BAZ")))
  mat_out <- matrix(c(1L, 5L, 6L), dimnames = list(c("a", "BAR", "BAZ")))
  expect_identical(
    vec_assign_params(
      mat_x,
      c(FALSE, TRUE, TRUE),
      mat_y,
      assign_names = TRUE,
      slice_value = TRUE
    ),
    mat_out
  )

  nested_x <- new_data_frame(
    list(df = df_x, mat = mat_x, vec = vec_x),
    row.names = c("foo", "bar", "baz")
  )
  nested_y <- new_data_frame(
    list(df = df_y, mat = mat_y, vec = vec_y),
    row.names = c(c("x", "y", "z"))
  )
  nested_out <- new_data_frame(
    list(df = df_out, mat = mat_out, vec = vec_out),
    row.names = c("foo", "y", "z")
  )

  expect_identical(
    vec_assign_params(
      nested_x,
      c(FALSE, TRUE, TRUE),
      nested_y,
      assign_names = TRUE,
      slice_value = TRUE
    ),
    nested_out
  )
})

test_that("can optionally assign names (OO case)", {
  # In case upstream attributes handling changes
  skip_on_cran()

  # `set_names()` must be on the inside, otherwise the POSIXlt object
  # gets a `balanced` attribute of `NA`
  oo_x <- as_posixlt(set_names(
    c("2020-01-01", "2020-01-02", "2020-01-03"),
    letters[1:3]
  ))
  oo_y <- as_posixlt(c(FOO = "2020-01-04"))
  oo_out <- as_posixlt(c(
    a = "2020-01-01",
    FOO = "2020-01-04",
    c = "2020-01-03"
  ))
  expect_identical(
    vec_assign_params(oo_x, 2, oo_y, assign_names = TRUE),
    oo_out
  )

  nested_x <- new_data_frame(
    list(oo = oo_x),
    row.names = c("foo", "bar", "baz")
  )
  nested_y <- new_data_frame(list(oo = oo_y), row.names = c("quux"))
  nested_out <- new_data_frame(
    list(oo = oo_out),
    row.names = c("foo", "quux", "baz")
  )

  expect_identical(
    vec_assign_params(nested_x, 2, nested_y, assign_names = TRUE),
    nested_out
  )
})

test_that("assigning names clears existing names even if the new value doesn't have any (#2019)", {
  x <- c(a = 1, b = 2)

  # Keeps existing names
  expect_identical(
    vec_assign_params(x, 1L, 0),
    c(a = 0, b = 2)
  )
  expect_identical(
    vec_assign_params(x, 1L, c(c = 0)),
    c(a = 0, b = 2)
  )

  # Clears or replaces names
  expect_identical(
    vec_assign_params(x, 1L, 0, assign_names = TRUE),
    set_names(c(0, 2), c("", "b"))
  )
  expect_identical(
    vec_assign_params(x, 1L, c(c = 0), assign_names = TRUE),
    set_names(c(0, 2), c("c", "b"))
  )
})

test_that("assignment requires that the value proxy is the same type as the output proxy", {
  x <- foobar(1)
  y <- foobar("a")

  local_foobar_proxy()
  local_methods(
    vec_cast.vctrs_foobar.vctrs_foobar = function(x, to, ...) x
  )

  expect_error(
    vec_assign(x, 1, y),
    "`double` incompatible with `value` proxy of type `character`"
  )
})

test_that("assignment allows a df `value`'s column to be a different type than its proxy (#1082)", {
  x <- new_data_frame(list(x = foobar(1)))
  y <- new_data_frame(list(x = foobar(2)))

  local_methods(
    # proxying foobar wraps it in a 1 col df
    vec_proxy.vctrs_foobar = function(x, ...) {
      attributes(x) <- NULL
      new_data_frame(list(vec = x))
    },
    # restoring extracts the column
    vec_restore.vctrs_foobar = function(x, to, ...) {
      foobar(x$vec)
    },
    vec_ptype2.vctrs_foobar.vctrs_foobar = function(x, y, ...) x
  )

  expect1 <- new_data_frame(list(x = foobar(c(1, 1))))
  expect2 <- new_data_frame(list(x = foobar(2)))

  expect_identical(vec_rbind(x, x), expect1)
  expect_identical(vec_assign(x, 1, y), expect2)
})

test_that("monitoring: assignment to a data frame with unshared columns doesn't overwrite (#986)", {
  x <- new_df_unshared_col()
  value <- new_data_frame(list(x = 2))
  expect <- new_data_frame(list(x = 1L))

  # - On R < 4.0.0, the NAMED value of the column is 0.
  # - On R >= 4.0.0, the refcnt of the column is 1 from the call to
  #   `SET_VECTOR_ELT()` in `new_df_unshared_col()`.
  expect_false(maybe_shared_col(x, 1L))

  new <- vec_assign(x, 1, value)

  # On R < 4.0.0, `vec_assign()` shallow duplicates `x`, which recursively
  # bumps the NAMED-ness of each column of `x` to the max value of 7 by
  # calling `ENSURE_NAMEDMAX()` on it. So the columns of `x` are all considered
  # shared from that.

  # On R >= 4.0.0, references are tracked more precisely.
  # - `new_df_unshared_col()` calls `SET_VECTOR_ELT()` when setting the
  #   column into `x`, bumping the column's namedness to 1.
  # - Then, at the start of `df_assign()`, `x` is shallow duplicated and
  #   assigned to `out`. This calls `ENSURE_NAMEDMAX()` on each column,
  #   however this does nothing on R 4.0.0. The refcnt of each column is instead
  #   incremented by 1 by calls to `SET_VECTOR_ELT()` in `duplicate1()`.
  #   So now it is at 2.
  # - But then in `df_assign()` we use `SET_VECTOR_ELT()` on `out`, overwriting
  #   each column. This actually decrements the refcnt on the value that was
  #   in `out` before the column was overwritten. The column of `out` that it
  #   decrements the refcnt for is the same SEXP as that column in `x`, so now
  #   it is back to 1, and it is not considered shared.

  if (getRversion() >= "4.0.0") {
    expect_false(maybe_shared_col(x, 1L))
  } else {
    expect_true(maybe_shared_col(x, 1L))
  }

  # Expect no changes to `x`!
  expect_identical(x, expect)
})

test_that("monitoring: assignment to atomic vectors doesn't modify by reference", {
  x <- c(1, 2, 3)
  expect <- c(1, 2, 3)

  vec_assign(x, 2, 3)

  expect_identical(x, expect)
})

test_that("monitoring: assignment to POSIXlt doesn't modify by reference (#1951)", {
  original <- as.POSIXlt("2020-11-01")
  expect_original <- as.POSIXlt("2020-11-01")

  value <- as.POSIXlt("2020-12-02")

  expect <- as.POSIXlt("2020-12-02")
  actual <- vec_assign(original, 1, value)
  expect_identical(expect, actual)

  expect_identical(original, expect_original)
})

test_that("monitoring: assignment to `vctrs_rcrd` doesn't modify by reference (#1951)", {
  original <- new_rcrd(list(x = 1:5))
  expect_original <- new_rcrd(list(x = 1:5))

  value <- new_rcrd(list(x = 0L))

  expect <- new_rcrd(list(x = c(0L, 2:5)))
  actual <- vec_assign(original, 1, value)
  expect_identical(expect, actual)

  expect_identical(original, expect_original)
})

# vec_assign + compact_seq -------------------------------------------------

test_that("can assign base vectors with compact seqs", {
  # `start` is 0-based
  start <- 1L
  size <- 2L
  increasing <- TRUE

  x <- c(FALSE, FALSE, FALSE)
  value <- c(TRUE, NA, TRUE)
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, c(FALSE, NA, TRUE))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- c(1L, 2L, 3L)
  value <- c(4L, 5L, 6L)
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, c(1L, 5L, 6L))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- c(1, 2, 3)
  value <- c(4, 5, 6)
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, c(1, 5, 6))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- c(1i, 2i, 3i)
  value <- c(4i, 5i, 6i)
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, c(1i, 5i, 6i))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- c("1", "2", "3")
  value <- c("4", "5", "6")
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, c("1", "5", "6"))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- raw2(1, 2, 3)
  value <- raw2(4, 5, 6)
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, raw2(1, 5, 6))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- list(1, 2, 3)
  value <- list(4, 5, 6)
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, list(1, 5, 6))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )
})

test_that("can assign shaped base vectors with compact seqs", {
  # `start` is 0-based
  start <- 1L
  size <- 2L
  increasing <- TRUE
  mat <- as.matrix

  x <- mat(c(FALSE, FALSE, FALSE))
  value <- mat(c(TRUE, NA, TRUE))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(c(FALSE, NA, TRUE)))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- mat(c(1L, 2L, 3L))
  value <- mat(c(4L, 5L, 6L))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(c(1L, 5L, 6L)))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- mat(c(1, 2, 3))
  value <- mat(c(4, 5, 6))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(c(1, 5, 6)))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- mat(c(1i, 2i, 3i))
  value <- mat(c(4i, 5i, 6i))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(c(1i, 5i, 6i)))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- mat(c("1", "2", "3"))
  value <- mat(c("4", "5", "6"))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(c("1", "5", "6")))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- mat(raw2(1, 2, 3))
  value <- mat(raw2(4, 5, 6))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(raw2(1, 5, 6)))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )

  x <- mat(list(1, 2, 3))
  value <- mat(list(4, 5, 6))
  value_sliced <- vec_slice_seq(value, start, size, increasing)
  y <- vec_assign_seq(x, value_sliced, start, size, increasing)
  expect_identical(y, mat(list(1, 5, 6)))
  expect_identical(
    vec_assign_seq(x, value, start, size, increasing, slice_value = TRUE),
    y
  )
})

test_that("can assign shaped base vectors with compact seqs and recycled `value`", {
  # `start` is 0-based
  start <- 1L
  size <- 2L
  increasing <- TRUE
  mat <- as.matrix
  expect_identical(
    vec_assign_seq(mat(lgl(1, 0, 1)), NA, start, size, increasing),
    mat(lgl(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(int(1, 2, 3)), NA, start, size, increasing),
    mat(int(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(dbl(1, 2, 3)), NA, start, size, increasing),
    mat(dbl(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(cpl2(1, 2, 3)), NA, start, size, increasing),
    mat(cpl2(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(chr("1", "2", "3")), NA, start, size, increasing),
    mat(chr("1", NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(raw2(1, 2, 3)), raw2(1), start, size, increasing),
    mat(raw2(1, 1, 1))
  )
  expect_identical(
    vec_assign_seq(mat(list(1, 2, 3)), NA, start, size, increasing),
    mat(list(1, NULL, NULL))
  )
})

test_that("can assign shaped base vectors with decreasing compact seqs and recycled `value`", {
  # `start` is 0-based
  start <- 2L
  size <- 2L
  increasing <- FALSE
  mat <- as.matrix
  expect_identical(
    vec_assign_seq(mat(lgl(1, 0, 1)), NA, start, size, increasing),
    mat(lgl(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(int(1, 2, 3)), NA, start, size, increasing),
    mat(int(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(dbl(1, 2, 3)), NA, start, size, increasing),
    mat(dbl(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(cpl2(1, 2, 3)), NA, start, size, increasing),
    mat(cpl2(1, NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(chr("1", "2", "3")), NA, start, size, increasing),
    mat(chr("1", NA, NA))
  )
  expect_identical(
    vec_assign_seq(mat(raw2(1, 2, 3)), raw2(1), start, size, increasing),
    mat(raw2(1, 1, 1))
  )
  expect_identical(
    vec_assign_seq(mat(list(1, 2, 3)), NA, start, size, increasing),
    mat(list(1, NULL, NULL))
  )
})

test_that("can assign shaped base vectors with size 0 compact seqs", {
  # `start` is 0-based
  start <- 1L
  size <- 0L
  increasing <- TRUE
  mat <- as.matrix

  expect_identical(
    vec_assign_seq(mat(lgl(1, 0, 1)), NA, start, size, increasing),
    mat(mat(lgl(1, 0, 1)))
  )
  expect_identical(
    vec_assign_seq(mat(int(1, 2, 3)), NA, start, size, increasing),
    mat(int(1, 2, 3))
  )
  expect_identical(
    vec_assign_seq(mat(dbl(1, 2, 3)), NA, start, size, increasing),
    mat(dbl(1, 2, 3))
  )
  expect_identical(
    vec_assign_seq(mat(cpl(1, 2, 3)), NA, start, size, increasing),
    mat(cpl(1, 2, 3))
  )
  expect_identical(
    vec_assign_seq(mat(chr("1", "2", "3")), NA, start, size, increasing),
    mat(chr("1", "2", "3"))
  )
  expect_identical(
    vec_assign_seq(mat(raw2(1, 2, 3)), raw2(1), start, size, increasing),
    mat(raw2(1, 2, 3))
  )
  expect_identical(
    vec_assign_seq(mat(list(1, 2, 3)), NA, start, size, increasing),
    mat(list(1, 2, 3))
  )
})

test_that("can assign object of any dimensionality with compact seqs", {
  x1 <- ones(3)
  x2 <- ones(3, 4)
  x3 <- ones(3, 4, 5)
  x4 <- ones(3, 4, 5, 6)

  # `start` is 0-based
  start <- 0L
  size <- 2L
  increasing <- TRUE
  mat <- as.matrix

  expect_identical(
    vec_assign_seq(x1, 2, start, size, increasing),
    array(rep(c(2, 2, 1), 1), dim = 3)
  )
  expect_identical(
    vec_assign_seq(x2, 2, start, size, increasing),
    array(rep(c(2, 2, 1), 4), dim = c(3, 4))
  )
  expect_identical(
    vec_assign_seq(x3, 2, start, size, increasing),
    array(rep(c(2, 2, 1), 20), dim = c(3, 4, 5))
  )
  expect_identical(
    vec_assign_seq(x4, 2, start, size, increasing),
    array(rep(c(2, 2, 1), 120), dim = c(3, 4, 5, 6))
  )
})

# vec_assign + compact_condition -----------------------------------------------

test_that("can assign base vectors with compact conditions", {
  i <- c(FALSE, TRUE, TRUE)
  i_compact <- as_compact_condition(i)

  x <- c(FALSE, FALSE, FALSE)
  value <- c(TRUE, NA, TRUE)
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, c(FALSE, NA, TRUE))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- c(1L, 2L, 3L)
  value <- c(4L, 5L, 6L)
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, c(1L, 5L, 6L))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- c(1, 2, 3)
  value <- c(4, 5, 6)
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, c(1, 5, 6))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- c(1i, 2i, 3i)
  value <- c(4i, 5i, 6i)
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, c(1i, 5i, 6i))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- c("1", "2", "3")
  value <- c("4", "5", "6")
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, c("1", "5", "6"))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- raw2(1, 2, 3)
  value <- raw2(4, 5, 6)
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, raw2(1, 5, 6))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- list(1, 2, 3)
  value <- list(4, 5, 6)
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, list(1, 5, 6))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )
})

test_that("can assign shaped base vectors with compact conditions", {
  i <- c(FALSE, TRUE, TRUE)
  i_compact <- as_compact_condition(i)

  mat <- as.matrix

  x <- mat(c(FALSE, FALSE, FALSE))
  value <- mat(c(TRUE, NA, TRUE))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(c(FALSE, NA, TRUE)))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- mat(c(1L, 2L, 3L))
  value <- mat(c(4L, 5L, 6L))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(c(1L, 5L, 6L)))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- mat(c(1, 2, 3))
  value <- mat(c(4, 5, 6))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(c(1, 5, 6)))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- mat(c(1i, 2i, 3i))
  value <- mat(c(4i, 5i, 6i))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(c(1i, 5i, 6i)))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- mat(c("1", "2", "3"))
  value <- mat(c("4", "5", "6"))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(c("1", "5", "6")))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- mat(raw2(1, 2, 3))
  value <- mat(raw2(4, 5, 6))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(raw2(1, 5, 6)))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )

  x <- mat(list(1, 2, 3))
  value <- mat(list(4, 5, 6))
  value_sliced <- vec_slice(value, i)
  y <- vec_assign_compact_condition(x, i_compact, value_sliced)
  expect_identical(y, mat(list(1, 5, 6)))
  expect_identical(
    vec_assign_compact_condition(x, i_compact, value, slice_value = TRUE),
    y
  )
})

test_that("can assign shaped base vectors with compact conditions and recycled `value`", {
  i <- as_compact_condition(c(FALSE, TRUE, TRUE))
  mat <- as.matrix

  expect_identical(
    vec_assign_compact_condition(mat(lgl(1, 0, 1)), i, NA),
    mat(lgl(1, NA, NA))
  )
  expect_identical(
    vec_assign_compact_condition(mat(int(1, 2, 3)), i, NA),
    mat(int(1, NA, NA))
  )
  expect_identical(
    vec_assign_compact_condition(mat(dbl(1, 2, 3)), i, NA),
    mat(dbl(1, NA, NA))
  )
  expect_identical(
    vec_assign_compact_condition(mat(cpl2(1, 2, 3)), i, NA),
    mat(cpl2(1, NA, NA))
  )
  expect_identical(
    vec_assign_compact_condition(mat(chr("1", "2", "3")), i, NA),
    mat(chr("1", NA, NA))
  )
  expect_identical(
    vec_assign_compact_condition(mat(raw2(1, 2, 3)), i, raw2(1)),
    mat(raw2(1, 1, 1))
  )
  expect_identical(
    vec_assign_compact_condition(mat(list(1, 2, 3)), i, NA),
    mat(list(1, NULL, NULL))
  )
})

test_that("can assign shaped base vectors with size 0 compact conditions", {
  i <- as_compact_condition(logical())
  mat <- as.matrix

  expect_identical(
    vec_assign_compact_condition(mat(lgl(1, 0, 1)), i, NA),
    mat(mat(lgl(1, 0, 1)))
  )
  expect_identical(
    vec_assign_compact_condition(mat(int(1, 2, 3)), i, NA),
    mat(int(1, 2, 3))
  )
  expect_identical(
    vec_assign_compact_condition(mat(dbl(1, 2, 3)), i, NA),
    mat(dbl(1, 2, 3))
  )
  expect_identical(
    vec_assign_compact_condition(mat(cpl(1, 2, 3)), i, NA),
    mat(cpl(1, 2, 3))
  )
  expect_identical(
    vec_assign_compact_condition(mat(chr("1", "2", "3")), i, NA),
    mat(chr("1", "2", "3"))
  )
  expect_identical(
    vec_assign_compact_condition(mat(raw2(1, 2, 3)), i, raw2(1)),
    mat(raw2(1, 2, 3))
  )
  expect_identical(
    vec_assign_compact_condition(mat(list(1, 2, 3)), i, NA),
    mat(list(1, 2, 3))
  )
})

test_that("can assign object of any dimensionality with compact conditions", {
  x1 <- ones(3)
  x2 <- ones(3, 4)
  x3 <- ones(3, 4, 5)
  x4 <- ones(3, 4, 5, 6)

  i <- as_compact_condition(c(TRUE, TRUE, FALSE))

  expect_identical(
    vec_assign_compact_condition(x1, i, 2),
    array(rep(c(2, 2, 1), 1), dim = 3)
  )
  expect_identical(
    vec_assign_compact_condition(x2, i, 2),
    array(rep(c(2, 2, 1), 4), dim = c(3, 4))
  )
  expect_identical(
    vec_assign_compact_condition(x3, i, 2),
    array(rep(c(2, 2, 1), 20), dim = c(3, 4, 5))
  )
  expect_identical(
    vec_assign_compact_condition(x4, i, 2),
    array(rep(c(2, 2, 1), 120), dim = c(3, 4, 5, 6))
  )
})
