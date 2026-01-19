# ------------------------------------------------------------------------------
# Unstructure - atomic

test_that("retains `names`", {
  x <- c(a = 1, b = 2)
  expect_named(vec_unstructure(x), c("a", "b"))
})

test_that("removes extraneous attributes", {
  x <- structure(1:5, names = letters[1:5], foo = "bar", class = "my_integer")
  expect <- set_names(1:5, letters[1:5])
  expect_identical(vec_unstructure(x), expect)
})

test_that("clears S4 bit on atomic S4s", {
  # The S4 object is "built on" an atomic double vector
  s4_atomic_double <- setClass(
    "vctrs_s4_atomic_double",
    contains = "numeric"
  )

  data <- c(1, 2, 3, 4, 5)

  x <- s4_atomic_double(data)
  expect_true(isS4(x))

  # Returns the underlying atomic double vector
  out <- vec_unstructure(x)
  expect_identical(out, data)
  expect_false(isS4(out))
})

test_that("can unstructure <list_of>", {
  x <- list_of(a = 1:5, b = 6:10, .ptype = integer(), .size = 5)
  expect <- list(a = 1:5, b = 6:10)
  expect_identical(vec_unstructure(x), expect)
})

test_that("no copies are made when not required", {
  x <- 1:5
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )

  x <- set_names(1:5, letters[1:5])
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )
})

# ------------------------------------------------------------------------------
# Unstructure - array

test_that("retains `dim` and `dimnames[[1]]`", {
  x <- array(
    1:6,
    dim = c(2L, 3L),
    dimnames = list(c("a", "b"), NULL)
  )

  out <- vec_unstructure(x)
  expect_identical(dim(out), c(2L, 3L))
  expect_identical(rownames(out), c("a", "b"))
  expect_identical(colnames(out), NULL)
})

test_that("removes extraneous attributes", {
  x <- structure(
    1:6,
    dim = c(2, 3),
    dimnames = list(c("a", "b"), c("c", "d", "e")),
    foo = "bar",
    class = "my_integer"
  )
  expect <- array(
    1:6,
    dim = c(2, 3),
    dimnames = list(c("a", "b"), NULL)
  )
  expect_identical(vec_unstructure(x), expect)
})

test_that("`dimnames` are correctly unstructured", {
  # Drop column names
  x <- array(1:4, c(2, 2))
  dimnames(x) <- list(c("a", "b"), c("c", "d"))
  expect_identical(
    dimnames(vec_unstructure(x)),
    list(c("a", "b"), NULL)
  )

  # Drop `dimnames` entirely
  x <- array(1:4, c(2, 2))
  dimnames(x) <- list(NULL, NULL)
  expect_identical(
    dimnames(vec_unstructure(x)),
    NULL
  )

  # Drop `dimnames` entirely
  x <- array(1:4, c(2, 2))
  dimnames(x) <- list(NULL, c("c", "d"))
  expect_identical(
    dimnames(vec_unstructure(x)),
    NULL
  )

  # Drop meta names
  x <- array(1:4, c(2, 2))
  dimnames(x) <- list(meta = c("a", "b"), meta2 = NULL)
  expect_identical(
    dimnames(vec_unstructure(x)),
    list(c("a", "b"), NULL)
  )

  # Drop `dimnames` entirely
  x <- array(1:4, c(2, 2))
  dimnames(x) <- list(meta = NULL, NULL)
  expect_identical(
    dimnames(vec_unstructure(x)),
    NULL
  )
})

test_that("no copies are made when not required", {
  x <- array(1:6)
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )

  x <- array(1:6, dimnames = list(letters[1:6]))
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )

  x <- array(1:6, dim = c(2, 3))
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )

  x <- array(1:6, dim = c(2, 3), dimnames = list(c("a", "b"), NULL))
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )
})

# ------------------------------------------------------------------------------
# Unstructure - data.frame

test_that("retains `names` and `row.names` and 'data.frame' class", {
  x <- data_frame(a = 1:5, b = 6:10)
  x <- vec_set_names(x, letters[1:5])

  out <- vec_unstructure(x)
  expect_identical(names(out), c("a", "b"))
  expect_identical(row.names(out), letters[1:5])
  expect_identical(class(out), "data.frame")
})

test_that("`row.names` remain in compact automatic form `c(NA, -n)`", {
  # Anything else would be too slow!

  # A data frame we don't have to modify
  x <- data_frame(x = 1:5)
  expect_identical(.row_names_info(x, 0L), c(NA, -5L))
  out <- vec_unstructure(x)
  expect_identical(.row_names_info(out, 0L), c(NA, -5L))

  # A data frame we do have to modify
  x <- data_frame(x = 1:5)
  attr(x, "foo") <- "bar"
  expect_identical(.row_names_info(x, 0L), c(NA, -5L))
  out <- vec_unstructure(x)
  expect_identical(.row_names_info(out, 0L), c(NA, -5L))
})

test_that("removes extraneous attributes", {
  x <- data_frame(a = 1:5, b = 6:10)
  attr(x, "foo") <- "bar"
  expect <- data_frame(a = 1:5, b = 6:10)
  expect_identical(vec_unstructure(x), expect)
})

test_that("removes extraneous classes", {
  x <- data_frame(a = 1:5, b = 6:10)
  class(x) <- c("my_df", "data.frame")
  expect <- data_frame(a = 1:5, b = 6:10)
  expect_identical(vec_unstructure(x), expect)
})

test_that("no copies are made when not required", {
  x <- data_frame(a = 1:5, b = 6:10)
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )

  x <- data_frame(a = 1:5, b = 6:10)
  x <- vec_set_names(x, letters[1:5])
  expect_identical(
    obj_address(x),
    obj_address(vec_unstructure(x))
  )
})

# ------------------------------------------------------------------------------
# Unstructure - misc

test_that("can unstructure <vctrs_rcrd>", {
  # Doesn't respect the proxy in any way, that's not what we are going for here
  data <- list(a = 1:5, b = 6:10)
  x <- new_rcrd(data, extra = "foo")
  expect_identical(vec_unstructure(x), data)
})

test_that("error class is correct", {
  expect_error(
    vec_unstructure(NULL),
    class = "vctrs_error_unsupported_storage_type"
  )
})

test_that("can't unstructure `NULL`", {
  expect_snapshot(error = TRUE, {
    vec_unstructure(NULL)
  })
})

test_that("can't unstructure non-vectors", {
  expect_snapshot(error = TRUE, {
    vec_unstructure(environment())
  })
})

test_that("can't unstructure non-atomic S4s", {
  # The S4 object is not built on any atomic type
  s4_not_atomic <- methods::setClass(
    "vctrs_s4_not_atomic",
    slots = c(name = "character", number = "numeric")
  )

  x <- s4_not_atomic(name = "the name", number = 42)
  expect_true(isS4(x))

  # Error message may depend on R version S4SXP vs OBJSXP, so don't capture it
  expect_error(vec_unstructure(x))
})
