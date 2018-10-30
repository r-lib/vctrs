context("test-type-factor")

test_that("ptype methods are descriptive", {
  f <- factor()
  o <- ordered(character())

  expect_equal(vec_ptype_abbr(f), "fctr")
  expect_equal(vec_ptype_abbr(o), "ord")

  expect_equal(vec_ptype_full(f), "factor<>")
  expect_equal(vec_ptype_full(o), "ordered<>")
})

# Coercion ----------------------------------------------------------------

test_that("factor/character coercions are symmetric and unnchanging", {
  types <- list(
    ordered(character()),
    factor(),
    character()
  )
  mat <- maxtype_mat(types)
  expect_true(isSymmetric(mat))

  expect_known_output(
    mat,
    print = TRUE,
    test_path("test-type-factor.txt"),
    width = 200
  )
})

test_that("factors level are unioned", {
  # This is technically incorrect, but because of R's existing behaviour
  # anything else will cause substantial friction.
  fa <- factor(levels = "a")
  fb <- factor(levels = "b")

  expect_equal(vec_type_common(fa, fb), factor(levels = c("a", "b")))
  expect_equal(vec_type_common(fb, fa), factor(levels = c("b", "a")))
})

# Casting -----------------------------------------------------------------

test_that("safe casts work as expected", {
  fa <- factor("a")
  fab <- factor(c("a", "b"))

  expect_equal(vec_cast(NULL, fa), NULL)

  expect_equal(vec_cast(fa, fa), fa)
  expect_equal(vec_cast(fa, fab), fab[1])
  expect_equal(vec_cast("a", fab), fab[1])

  expect_equal(vec_cast("a", factor()), fa)
  expect_equal(vec_cast(fa, factor()), fa)

  expect_equal(vec_cast(list("a", "b"), fab), fab)
})

test_that("can cast to character", {
  expect_equal(vec_cast(factor("X"), character()), "X")
  expect_equal(vec_cast(ordered("X"), character()), "X")
})

test_that("can cast NA to factor", {
  expect_equal(vec_cast(NA, new_factor()), factor(NA))
  expect_equal(vec_cast(NA, new_ordered()), ordered(NA))
})

test_that("lossy casts generate warning", {
  fa <- factor("a")
  fb <- factor("b")

  expect_condition(vec_cast(fa, fb), class = "warning_lossy_cast")
  expect_condition(vec_cast("a", fb), class = "warning_lossy_cast")
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(double(), factor("a")), class = "error_incompatible_cast")
})

test_that("orderedness of factor is preserved", {
  fct <- factor("a")
  ord <- ordered("a")

  expect_equal(vec_cast(fct, ord), ord)
  expect_equal(vec_cast("a", ord), ord)
})

test_that("NA are not considered lossy in factor cast (#109)", {
  f <- factor(c("itsy", "bitsy", NA, "spider", "spider"))
  expect_warning(vec_cast(f, f[1]), NA)
})

# Arithmetic and factor ---------------------------------------------------

test_that("factors don't support math or arthimetic", {
  f <- factor("x")
  expect_error(vec_math("sum", f), class = "error_unsupported")
  expect_error(vec_arith("+", f, f), class = "error_unsupported")
})
