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
