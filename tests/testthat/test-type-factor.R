context("test-type-factor")

test_that("ptype methods are descriptive", {
  f <- factor()
  o <- ordered(character())

  expect_equal(vec_ptype_abbr(f), "fct")
  expect_equal(vec_ptype_abbr(o), "ord")

  expect_equal(vec_ptype_full(f), "factor<>")
  expect_equal(vec_ptype_full(o), "ordered<>")
})

# Coercion ----------------------------------------------------------------

test_that("factor/character coercions are symmetric and unchanging", {
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

  expect_equal(vec_ptype_common(fa, fb), factor(levels = c("a", "b")))
  expect_equal(vec_ptype_common(fb, fa), factor(levels = c("b", "a")))
})

test_that("coercion errors with factors", {
  f <- factor(levels = "a")

  expect_error(vec_ptype_common(f, logical()), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype_common(logical(), f), class = "vctrs_error_incompatible_type")
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

test_that("can cast NA and unspecified to factor", {
  expect_identical(vec_cast(NA, new_factor()), factor(NA))
  expect_identical(vec_cast(NA, new_ordered()), ordered(NA))
  expect_identical(vec_cast(unspecified(2), new_factor()), factor(c(NA, NA)))
  expect_identical(vec_cast(unspecified(2), new_ordered()), ordered(c(NA, NA)))
})

test_that("lossy factor casts fail", {
  fa <- factor("a")
  fb <- factor("b")

  expect_lossy(vec_cast(fa, fb), factor(NA, levels = "b"), x = fa, to = fb)
  expect_lossy(vec_cast("a", fb), factor(NA, levels = "b"), x = chr(), to = fb)
})

test_that("invalid casts generate error", {
  expect_error(vec_cast(double(), factor("a")), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(factor("a"), logical()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(ordered("a"), logical()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(logical(), factor("a")), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(logical(), ordered("a")), class = "vctrs_error_incompatible_cast")
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
  expect_error(vec_math("sum", f), class = "vctrs_error_unsupported")
  expect_error(vec_arith("+", f, f), class = "vctrs_error_unsupported")
})
