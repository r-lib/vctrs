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

test_that("factors level are unioned", {
  # This is technically incorrect, but because of R's existing behaviour
  # anything else will cause substantial friction.
  fa <- vec_ptype(factor(levels = "a"))
  fb <- vec_ptype(factor(levels = "b"))

  expect_equal(vec_ptype(fa, fb), vec_ptype(factor(levels = c("a", "b"))))
  expect_equal(vec_ptype(fb, fa), vec_ptype(factor(levels = c("b", "a"))))
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

# Arithmetic and factor ---------------------------------------------------

test_that("factors don't support math or arthimetic", {
  f <- factor("x")
  expect_error(vec_math("sum", f), class = "error_unsupported")
  expect_error(vec_arith("+", f, f), class = "error_unsupported")
})
