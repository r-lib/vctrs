context("test-utils")

test_that("names preserved if outer name is missing", {
  x <- c("a", "z", "")

  expect_equal(outer_names(x, NULL, 3), x)
  expect_equal(outer_names(x, "", 3), x)
  expect_equal(outer_names(x, na_chr, 3), x)
})

test_that("outer name vectorised if needed", {
  expect_equal(outer_names(NULL, "x", 1L), c("x"))
  expect_equal(outer_names(NULL, "x", 2L), c("x1", "x2"))
})

test_that("outer and inner names are combined", {
  expect_equal(outer_names("x", "y", 1), c("y..x"))
})

test_that("options are created", {
  expect_identical(
    unclass(new_opts(c("a", "c"), letters[1:4])),
    c(a = TRUE, b = FALSE, c = TRUE, d = FALSE)
  )
})

test_that("can't supply unknown option", {
  expect_error(
    new_opts(c("a", "foo"), letters[1:4]),
    "Argument must be one of \"a\", \"b\", \"c\" or \"d\""
  )
  expect_error(
    new_opts(c("a", "foo"), letters[1:4], arg = "foo"),
    "`foo` must be one of \"a\", \"b\", \"c\" or \"d\""
  )
})

test_that("`has_dim()` doesn't partial match on the `dim` attribute (#948)", {
  x <- structure(1, dimB = 1)
  expect_false(has_dim(x))
})

test_that("df_has_base_subset() detects `[` methods", {
  expect_true(df_has_base_subset(foobar(mtcars)))

  out <- with_methods(
    `[.vctrs_foobar` = function(x, i, ...) structure(NextMethod(), dispatched = TRUE),
    df_has_base_subset(foobar(mtcars))
  )
  expect_false(out)
})

test_that("vec_common_suffix() finds common suffix", {
  x <- c("foo", "bar", "baz")
  y <- c("quux", "foo", "hop", "baz")
  expect_identical(vec_common_suffix(x, y), "baz")

  x <- c("foo", "bar", "baz")
  y <- c("quux", "foo", "bar", "baz")
  expect_identical(vec_common_suffix(x, y), x)

  x <- letters
  y <- chr()
  expect_identical(vec_common_suffix(x, y), chr())

  x <- data.frame(x = 1:3, y = c("foo", "bar", "baz"))
  y <- data.frame(x = 0:3, y = c("foo", "hop", "bar", "baz"))
  exp <- data.frame(x = 2:3, y = c("bar", "baz"))
  expect_identical(vec_common_suffix(x, y), exp)
})

test_that("fast_c() concatenates", {
  expect_identical(fast_c(character(), "foo"), "foo")
  expect_identical(fast_c("foo", character()), "foo")
  expect_identical(fast_c("foo", c("bar", "baz")), c("foo", "bar", "baz"))
  expect_identical(fast_c(c("bar", "baz"), "foo"), c("bar", "baz", "foo"))
})
