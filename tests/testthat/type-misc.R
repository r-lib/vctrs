
test_that("`numeric_version` is a vector (#723)", {
  x <- numeric_version("0.1.0")
  y <- numeric_version("0.2.0")
  z <- c(x, y)

  expect_true(vec_is(x))

  expect_true(vec_equal(x, x))
  expect_false(vec_equal(x, y))
  expect_identical(vec_equal(y, z), c(FALSE, TRUE))

  expect_identical(vec_unique(z), z)
  expect_identical(vec_unique(c(y, z, x)), z[2:1])
})

test_that("`numeric_version` falls back to base methods", {
  x <- utils::packageVersion("rlang")
  y <- utils::packageVersion("vctrs")
  z <- c(x, y)

  # `z` is a `list-of`-like type but slicing 1 element returns the
  # atomic type. To implement this in vctrs we'd need to provide a way
  # of customising the "wrapper" type for size > 1 vectors.
  expect_identical(vec_slice(z, 1:2), z)
  expect_identical(vec_slice(z, 1), x)
  expect_identical(vec_slice(z, 2), y)

  expect_identical(vec_c(x, y), z)
})
