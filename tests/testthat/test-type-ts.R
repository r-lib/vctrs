test_that("can take the ptype of `ts` vectors", {
  x <- ts(1:3)
  expect_identical(vec_ptype(x), integer())

  x <- ts(1.5)
  expect_identical(vec_ptype(x), double())

  x <- ts(matrix(1L, nrow = 2))
  expect_identical(vec_ptype(x), array(integer(), dim = c(0L, 1L)))
})

test_that("can slice `ts` vectors", {
  # Slicing `ts` with `[` drops the `ts` class
  x <- ts(1:3)
  expect_identical(vec_slice(x, 2), x[2])
  expect_identical(vec_slice(x, 2), 2L)
})

test_that("can't do a self cast", {
  x <- ts(1:3)
  y <- ts(4:7)

  # Not allowed, attributes can't be resolved
  expect_snapshot(error = TRUE, {
    vec_cast(x, y)
  })

  # This results in common type of integer, the underlying storage type
  expect_identical(
    vec_cast_common(x, y),
    list(1:3, 4:7)
  )

  # This actually works because the `vec_ptype()` returns the underlying
  # storage type, and the `vec_ptype()` of `.to` is taken
  expect_identical(
    vec_cast_common(x, y, .to = x),
    list(1:3, 4:7)
  )
})

test_that("can concatenate `ts` vectors", {
  # `c()` method demonstrates that the common type is the common type of the
  # underlying storage type, and attributes are dropped
  x <- ts(1:3)
  expect_identical(vec_c(x, x), c(x, x))

  df <- data_frame(x = x)
  expect_identical(vec_rbind(df, df), data_frame(x = c(x, x)))

  y <- ts(c(4, 5, 6))
  expect_identical(vec_c(x, y), c(x, y))

  # vctrs rules retain the shape here
  z <- ts(matrix(c(4, 5, 6)))
  expect_identical(vec_c(x, z), matrix(c(1, 2, 3, 4, 5, 6)))
  expect_identical(c(x, z), c(1, 2, 3, 4, 5, 6))
})
