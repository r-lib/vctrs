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
  mat_strct <- maxtype_mat(types)
  expect_true(isSymmetric(mat_strct))

  mat_relax <- maxtype_mat(types, "relax")
  expect_true(isSymmetric(mat_relax))

  expect_known_output(
    {
      print(mat_strct)
      cat("\n\n")
      print(mat_relax)
    },
    test_path("test-type-coerce-factor.txt"),
    width = 200
  )

})

# Factors -----------------------------------------------------------------

test_that("factors can be coerced to character when relaxed", {
  fa <- vec_type(factor("a"))
  fb <- vec_type(factor("b"))

  type <- max(fa, fb, strict = FALSE)
  expect_equal(type$prototype, character())
})

test_that("nested factors are equivalent", {
  fab <- vec_type(factor("a", "b"))
  fb <- vec_type(factor("b"))

  type <- max(fab, fb, strict = FALSE)
  expect_equal(type, fab)
})


# Data frame --------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- vec_type(data.frame(x = 1))

  expect_equal(max(dt, NULL), dt)
  expect_error(max(dt, 1:10), "No maximum type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- vec_type(data.frame(x = FALSE, y = 1L))
  dt2 <- vec_type(data.frame(x = 1.5, y = 1.5))

  expect_equal(max(dt1, dt2), dt2)
})

test_that("data frame combines variables", {
  dt1 <- vec_type(data.frame(x = 1))
  dt2 <- vec_type(data.frame(y = 1))

  dt3 <- max(dt1, dt2)
  expect_equal(dt3$prototype, data.frame(x = double(), y = double()))
})
