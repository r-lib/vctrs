
test_that("ptype methods are descriptive", {
  f <- factor()
  o <- ordered(character())

  expect_equal(vec_ptype_abbr(f), "fct")
  expect_equal(vec_ptype_abbr(o), "ord")

  expect_equal(vec_ptype_full(f), "factor<>")
  expect_equal(vec_ptype_full(o), "ordered<>")
})

test_that("slicing factors uses a proxy to not go through `[.factor`", {
  x <- factor("x")
  y <- ordered("y")

  local_methods(
    `[.factor` = function(x, ...) abort("should not be called")
  )

  expect_identical(vec_slice(x, 1), x)
  expect_identical(vec_slice(y, 1), y)
})

test_that("`vec_c()` throws the right error with subclassed factors (#1015)", {
  skip("Factors now have a `c()` method")

  a <- subclass(factor("a"))
  b <- subclass(factor("b"))

  # We used to return a subclass
  expect_identical(vec_c(a, a), subclass(factor(c("a", "a"))))

  # We used to fail if attributes were incoPatible
  expect_error(vec_c(a, b), class = "vctrs_error_incompatible_type")
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

  local_options(width = 200)
  expect_snapshot(print(mat))
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

test_that("vec_ptype2(<factor>, NA) is symmetric (#687)", {
  fct <- new_factor()
  expect_identical(
    vec_ptype2(fct, NA),
    vec_ptype2(NA, fct)
  )

  fct <- new_ordered()
  expect_identical(
    vec_ptype2(fct, NA),
    vec_ptype2(NA, fct)
  )
})

test_that("vec_ptype2(<integer64>, NA) is symmetric (#687)", {
  i64 <- bit64::integer64()
  expect_identical(
    vec_ptype2(i64, NA),
    vec_ptype2(NA, i64)
  )
})

test_that("vec_ptype2() errors with malformed factors", {
  x <- structure(1, class = "factor")
  y <- factor("x")

  expect_error(vec_ptype2(x, y, x_arg = "z"), "`z` is a corrupt factor")
  expect_error(vec_ptype2(y, x, y_arg = "z"), "`z` is a corrupt factor")
})

test_that("vec_ptype2() errors with malformed ordered factors", {
  x <- structure(1, class = c("ordered", "factor"))
  y <- as.ordered(factor("x"))

  expect_error(vec_ptype2(x, y, x_arg = "z"), "`z` is a corrupt ordered factor")
  expect_error(vec_ptype2(y, x, y_arg = "z"), "`z` is a corrupt ordered factor")
})

test_that("ordered factors with different levels are not compatible", {
  expect_error(
    vec_ptype2(ordered("a"), ordered("b")),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(ordered("a"), ordered(c("a", "b"))),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(ordered("a"), ordered("b")),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(ordered("a"), ordered(c("a", "b"))),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("factors and ordered factors are not compatible", {
  expect_error(
    vec_ptype2(factor("a"), ordered("a")),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(ordered("a"), factor("a")),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(factor("a"), ordered("a")),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(ordered("a"), factor("a")),
    class = "vctrs_error_incompatible_type"
  )
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

  # This used to be allowed
  expect_error(vec_cast(list("a", "b"), fab), class = "vctrs_error_incompatible_type")
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
  expect_error(vec_cast(double(), factor("a")), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(factor("a"), logical()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(ordered("a"), logical()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(logical(), factor("a")), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(logical(), ordered("a")), class = "vctrs_error_incompatible_type")
})

test_that("orderedness of factor is preserved", {
  ord <- ordered(c("a", "b"), levels = c("b", "a"))
  expect_equal(vec_cast("a", ord), ordered("a", levels = c("b", "a")))
})

test_that("NA are not considered lossy in factor cast (#109)", {
  f <- factor(c("itsy", "bitsy", NA, "spider", "spider"))
  expect_warning(vec_cast(f, f[1]), NA)
})

test_that("Casting to a factor with explicit NA levels retains them", {
  f <- factor(c("x", NA), exclude = NULL)
  expect_identical(vec_cast(f, f), f)
  expect_identical(vec_cast(f, factor()), f)
})

test_that("characters can be cast to ordered", {
  expect_identical(vec_cast("a", ordered("a")), ordered("a"))
  expect_error(vec_cast(c("a", "b"), ordered("a")), class = "vctrs_error_cast_lossy")
})


# Proxy / restore ---------------------------------------------------------

test_that("subclassed factors / ordered factors can be restored (#1015)", {
  x <- subclass(factor("a"))
  proxy <- vec_proxy(x)
  expect_identical(vec_restore(proxy, x), x)

  y <- subclass(ordered("a"))
  proxy <- vec_proxy(y)
  expect_identical(vec_restore(proxy, y), y)
})

# Arithmetic and factor ---------------------------------------------------

test_that("factors don't support math or arthimetic", {
  f <- factor("x")
  expect_error(vec_math("sum", f), class = "vctrs_error_unsupported")
  expect_error(vec_arith("+", f, f), class = "vctrs_error_unsupported")
})
