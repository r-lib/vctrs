context("test-type-coerce")

# High-level regression + symmetry tests ---------------------------------------------

test_that("base coercions are symmetric and unchanging", {
  types <- list(NULL, logical(), integer(), double(), character(), list())
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(mat, test_path("test-type-coerce-base.txt"), print = TRUE)
})

test_that("datetime coercions are symmetric and unchanging", {
  types <- list(
    NULL,
    Sys.Date(),
    Sys.time(),
    difftime(Sys.time() + 1000, Sys.time()),
    difftime(Sys.time() + 1, Sys.time())
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(
    mat,
    test_path("test-type-coerce-date-time.txt"),
    print = TRUE,
    width = 200
  )
})

test_that("factor/character coercions are symmetric and unnchanging", {
  types <- list(
    NULL,
    fa = factor(levels = c("a")),
    fab = factor(levels = c("a", "b")),
    fc = factor(levels = c("c")),
    character()
  )
  mat <- maxtype_mat(types)

  expect_true(isSymmetric(mat))
  expect_known_output(
    mat,
    test_path("test-type-coerce-factor.txt"),
    print = TRUE,
    width = 200
  )

})

# Factors -----------------------------------------------------------------

test_that("factors can be coerced to character when relaxed", {
  fa <- vec_type(factor("a"))
  fb <- vec_type(factor("b"))

  type <- max(fa, fb, strict = FALSE)
  expect_equal(type$prototype, character())

  type <- max(fa, "x", strict = FALSE)
  expect_equal(type$prototype, character())
})

test_that("nested factors are equivalent", {
  fab <- vec_type(factor("a", "b"))
  fb <- vec_type(factor("b"))

  type <- max(fab, fb, strict = FALSE)
  expect_equal(type, fab)
})



