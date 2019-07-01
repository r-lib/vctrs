context("vctrs")

test_that("generics are extensible", {
  expect_error(vec_cast(NA, NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_restore(NA, NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_proxy(NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_proxy_equal(NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_proxy_compare(NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_ptype2(NA, NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_ptype_abbr(NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_ptype_full(NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_arith(NA, NA, NA, NA), class = "rlib_error_dots_nonempty")
  expect_error(vec_ptype_finalise(NA, NA), class = "rlib_error_dots_nonempty")
})
