context("test-prototype")


test_that("vec_type fails for non-vectors", {
  expect_error(
    vec_type(quote(name)),
    "`x` is not a vector",
    fixed = TRUE
  )
})

test_that("vec_type(NULL) returns NULL", {
  expect_identical(vec_type(NULL), NULL)
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

test_that("unspecified prototypes created from under specified inputs", {
  expect_equal(vec_type_common(), NULL)
  expect_equal(vec_type_common(NULL), NULL)

  expect_equal(vec_type_common(NA), logical())
  expect_equal(vec_type_common(NA, NULL), logical())
  expect_equal(vec_type_common(NULL, NA), logical())
})

test_that("unspecified prototypes created from data frame cols", {
  df <- data.frame(x = NA)
  expect_equal(vec_type_common(df)$x, unspecified())
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

