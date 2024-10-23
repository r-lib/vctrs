
# common shape ------------------------------------------------------------

test_that("vec_shape2() applies recycling rules", {
  expect_equal(vec_shape2(shaped_int(1, 5, 5), shaped_int(1)),       c(0L, 5L, 5L))
  expect_equal(vec_shape2(shaped_int(1),       shaped_int(1, 5, 5)), c(0L, 5L, 5L))
  expect_equal(vec_shape2(shaped_int(1, 1),    shaped_int(1, 5, 5)), c(0L, 5L, 5L))
  expect_equal(vec_shape2(shaped_int(1, 1, 1), shaped_int(1, 5, 5)), c(0L, 5L, 5L))

  expect_equal(vec_shape2(shaped_int(1, 1, 5), shaped_int(1, 5, 1)), c(0L, 5L, 5L))
  expect_equal(vec_shape2(shaped_int(1, 5, 1), shaped_int(1, 1, 5)), c(0L, 5L, 5L))
  expect_equal(vec_shape2(shaped_int(1, 1, 1), shaped_int(1, 5, 5)), c(0L, 5L, 5L))

  expect_equal(vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 1, 1)), c(0L, 0L, 5L))
})

test_that("incompatible shapes throw errors", {
  expect_snapshot({
    (expect_error(vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 5, 1)), class = "vctrs_error_incompatible_type"))
    (expect_error(vec_shape2(shaped_int(1, 5, 0), shaped_int(1, 1, 5)), class = "vctrs_error_incompatible_type"))
  })
})

test_that("can override error args", {
  expect_snapshot({
    (expect_error(
      vec_shape2(shaped_int(1, 0, 5), shaped_int(1, 5, 1), x_arg = "foo", y_arg = "bar"),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("vec_shape2() evaluates arg lazily", {
  expect_silent(vec_shape2(shaped_int(1, 5, 5), shaped_int(1), x_arg = print("oof")))
  expect_silent(vec_shape2(shaped_int(1, 5, 5), shaped_int(1), y_arg = print("oof")))
})

# broadcasting -------------------------------------------------------------

test_that("can broadcast to higher dimension, but not lower", {
  expect_identical(shape_broadcast_(1, NULL), 1)
  expect_null(shape_broadcast_(NULL, 1))

  expect_equal(
    shape_broadcast_(1, shaped_int(0, 4)),
    array(1, c(1, 4))
  )
  expect_error(
    shape_broadcast_(shaped_int(1, 1, 1), shaped_int(4, 4)),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    shape_broadcast_(shaped_int(3, 2), shaped_int(3, 3)),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("shape_broadcast_() applies recycling rules", {
  expect_equal(
    shape_broadcast_(array(1:4, c(1, 1, 4)), shaped_int(0, 4, 4))[1, , ],
    matrix(1:4, 4, 4, byrow = TRUE)
  )

  expect_equal(
    shape_broadcast_(array(1:4, c(1, 4, 1)), shaped_int(0, 4, 4))[1, , ],
    matrix(1:4, 4, 4)
  )

  expect_equal(
    shape_broadcast_(array(1L, c(1, 1)), shaped_int(1, 0)),
    matrix(integer(), nrow = 1)
  )

  expect_error(
    shape_broadcast_(array(1L, c(1, 2)), shaped_int(1, 0)),
    "Non-recyclable dimensions",
    class = "vctrs_error_incompatible_type"
  )

  expect_error(
    shape_broadcast_(array(1L, c(1, 0)), shaped_int(1, 1)),
    "Non-recyclable dimensions",
    class = "vctrs_error_incompatible_type"
  )
})

test_that("can combine shaped native classes (#1290, #1329)", {
  x <- new_datetime(c(1, 1e6))
  dim(x) <- c(1, 2)
  out <- vec_c(x, x)

  expect_s3_class(out, c("POSIXct", "POSIXt"))
  expect_dim(out, c(2, 2))

  y <- new_datetime(1:3 + 0.0)
  dim(y) <- c(1, 3)

  expect_snapshot(error = TRUE, vec_c(x, y))

  d <- structure(Sys.Date(), dim = 1)
  expect_equal(
    vec_rbind(data.frame(d), data.frame(d)),
    data.frame(d = structure(rep(Sys.Date(), 2), dim = 2))
  )
})

test_that("factor casts support shape", {
  x <- factor(c("x", "y", "z"))
  dim(x) <- c(3, 1)
  dimnames(x) <- list(c("r1", "r2", "r3"), "c1")

  y <- factor(c("w", "x", "y", "z"))
  dim(y) <- c(2, 2)

  exp <- factor(
    c("x", "y", "z", "x", "y", "z"),
    levels = c("w", "x", "y", "z")
  )
  dim(exp) <- c(3, 2)
  dimnames(exp) <- list(c("r1", "r2", "r3"), c("c1", "c1"))

  expect_equal(vec_cast(x, y), exp)

  x <- factor(c("x", "y", "z"))
  dim(x) <- c(3, 1)
  y <- factor(c("x", "y", "z"))
  expect_snapshot(error = TRUE, vec_cast(x, y))
})
