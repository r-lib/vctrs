context("test-c")

test_that("zero length input returns NULL", {
  expect_equal(vec_c(), NULL)
  expect_equal(vec_c(NULL), NULL)
  expect_equal(vec_c(NULL,), NULL)
  expect_equal(vec_c(NULL, NULL), NULL)
})

test_that("NULL is idempotent", {
  expect_equal(vec_c(NULL, 1L), 1L)
  expect_equal(vec_c(1L, NULL), 1L)
})

test_that("NA is idempotent", {
  expect_equal(vec_c(NA, 1L), c(NA, 1L))
  expect_equal(vec_c(NA, "x"), c(NA, "x"))
  expect_equal(vec_c(NA, factor("x")), factor(c(NA, "x")))
  expect_equal(vec_c(NA, new_date(0)), new_date(c(NA, 0)))
  expect_equal(vec_c(NA, new_datetime(0)), new_datetime(c(NA, 0)))
  expect_equal(vec_c(NA, new_duration(0)), new_duration(c(NA, 0)))
})

test_that("NA is logical if no other types intervene", {
  expect_equal(vec_c(logical()), logical())
  expect_equal(vec_c(NA), NA)
  expect_equal(vec_c(NA, NA), c(NA, NA))
})


test_that("different types are coerced to common", {
  expect_equal(vec_c(TRUE, 1L, 1), c(1, 1, 1))
  expect_equal(vec_c(TRUE, 2:4), 1:4)
})

test_that("specified .ptypes allows more casts", {
  expect_equal(vec_c(TRUE, .ptype = character()), "TRUE")
})

test_that("combines outer an inner names", {
  expect_equal(vec_c(x = 1), c(x = 1))
  expect_equal(vec_c(c(x = 1)), c(x = 1))

  expect_equal(vec_c(c(x = 1:2)), c(x1 = 1, x2 = 2))
  expect_error(vec_c(y = c(x = 1)), "Please supply")
})

test_that("can bind data.frame columns", {
  df <- data.frame(x = NA, y = 1:2)
  df$x <- data.frame(a = 1:2)

  expected <- data.frame(x = NA, y = c(1:2, 1:2))
  expected$x <- data.frame(a = c(1:2, 1:2))

  expect_equal(vec_c(df, df), expected)
})

test_that("vec_c() handles matrices", {
  m <- matrix(1:4, nrow = 2)
  dimnames(m) <- list(c("foo", "bar"), c("baz", "quux"))

  # FIXME: `vec_ptype_common(m, m)` doesn't return dimension names
  exp <- matrix(c(1:2, 1:2, 3:4, 3:4), nrow = 4)
  rownames(exp) <- c("foo", "bar", "foo", "bar")

  expect_identical(vec_c(m, m), exp)
})

test_that("vec_c() includes index in argument tag", {
  df1 <- tibble(x = tibble(y = tibble(z = 1)))
  df2 <- tibble(x = tibble(y = tibble(z = "a")))

  expect_known_output(file = test_path("test-type-vec-c-error.txt"), {
    try2(vec_c(df1, df2))
    try2(vec_c(df1, df1, df2))
    try2(vec_c(foo = df1, bar = df2))
  })
})

test_that("vec_c() handles record classes", {
  scoped_rational_class()

  out <- vec_c(rational(1, 2), 1L, NA)

  expect_true(vec_is(out, rational(1, 2)))
  expect_size(out, 3)
  expect_identical(vec_proxy(out), data.frame(n = c(1L, 1L, NA), d = c(2L, 1L, NA)))
})

test_that("can mix named and unnamed vectors (#271)", {
  expect_identical(vec_c(c(a = 1), 2), c(a = 1, 2))
  expect_identical(vec_c(0, c(a = 1), 2, b = 3), c(0, a = 1, 2, b =3))
})

test_that("vec_c() repairs names", {
  # Default minimal repair
  expect_named(vec_c(a = 1, a = 2, `_` = 3), c("a", "a", "_"))
  out <- vec_c(!!!set_names(1, NA))
  expect_named(out, "")

  expect_named(vec_c(a = 1, a = 2, `_` = 3, .name_repair = "unique"), c("a...1", "a...2", "_"))
  expect_error(vec_c(a = 1, a = 2, `_` = 3, .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")

  expect_named(vec_c(a = 1, a = 2, `_` = 3, .name_repair = "universal"), c("a...1", "a...2", "._"))
})
