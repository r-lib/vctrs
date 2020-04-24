
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

test_that("common type of data.table and data.frame/tibble is data.table", {
  # As data.table is not in Suggests, these checks are only run on the
  # devs' machines
  skip_if_not_installed("data.table")
  import_from("data.table", "data.table")

  expect_identical(
    vec_ptype2(data.table(x = TRUE), data.frame(y = 2)),
    data.table(x = lgl(), y = dbl())
  )
  expect_identical(
    vec_ptype2(data.frame(y = 2), data.table(x = TRUE)),
    data.table(y = dbl(), x = lgl())
  )

  expect_identical(
    vec_cast(data.frame(y = 2), data.table(x = TRUE, y = 1L)),
    data.table(x = NA, y = 2L)
  )
  expect_identical(
    vec_cast(data.table(y = 2), data.frame(x = TRUE, y = 1L)),
    data.frame(x = NA, y = 2L)
  )

  expect_identical(
    vec_ptype2(data.table(x = TRUE), tibble(y = 2)),
    data.table(x = lgl(), y = dbl())
  )
  expect_identical(
    vec_ptype2(tibble(y = 2), data.table(x = TRUE)),
    data.table(y = dbl(), x = lgl())
  )

  expect_identical(
    vec_cast(tibble(y = 2), data.table(x = TRUE, y = 1L)),
    data.table(x = NA, y = 2L)
  )
  expect_identical(
    vec_cast(data.table(y = 2), tibble(x = TRUE, y = 1L)),
    data.frame(x = NA, y = 2L)
  )
})
