context("test-type")


test_that("vec_type() is a no-op for non-vectors", {
  expect_null(vec_type(NULL))
  expect_identical(vec_type(quote(name)), quote(name))
  expect_identical(vec_type(partial_frame(x = 1)), partial_frame(x = 1))
})

test_that(".ptype argument overrides others", {
  expect_equal(vec_type_common(.ptype = 1:10), numeric())
})

test_that(".ptype required in strict mode", {
  old <- options(vctrs.no_guessing = TRUE)
  on.exit(options(old))

  expect_error(vec_type_common(), "strict mode")
})

test_that("can feed ptype into itself", {
  expect_equal(vec_type_common(vec_type_common(1:10)), numeric())
})

test_that("finalised prototypes created from under specified inputs", {
  expect_equal(vec_type_common(), NULL)
  expect_equal(vec_type_common(NULL), NULL)

  expect_equal(vec_type_common(NA), logical())
  expect_equal(vec_type_common(NA, NULL), logical())
  expect_equal(vec_type_common(NULL, NA), logical())
})

test_that("finalised prototypes created from under specified data frame cols", {
  df <- data.frame(x = NA)
  expect_equal(vec_type_common(df)$x, logical())
})

test_that("non-missing logical get correct type", {
  expect_equal(vec_type_common(TRUE), logical())
})

test_that("output tests", {
  expect_known_output(vec_ptype(), "out/vec-ptype-0.txt")
  expect_known_output(vec_ptype(integer()), "out/vec-ptype-1.txt")
  expect_known_output(vec_ptype(integer(), double()), "out/vec-ptype-2.txt")
  expect_known_output(vec_ptype(logical(), integer(), double()), "out/vec-ptype-3.txt")
})

test_that("vec_type_common() handles matrices", {
  m <- matrix(1:4, nrow = 2)
  expect_identical(vec_type_common(m, m), matrix(int(), ncol = 2))
})
