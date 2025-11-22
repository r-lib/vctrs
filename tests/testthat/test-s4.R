test_that("basics", {
  x <- rando(10)

  expect_true(vec_is(x))
  expect_equal(vec_size(x), 10)
  expect_identical(vec_ptype_common(x, x), vec_ptype(x))
})

test_that("casting of rando works", {
  x <- as_rando(1:10)
  expect_equal(vec_cast(x, rando()), x)

  expect_equal(vec_cast(NA, rando()), as_rando(NA))
  expect_equal(vec_cast(unspecified(2), rando()), as_rando(c(NA, NA)))

  expect_error(vec_cast(x, factor()), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(factor(), x), class = "vctrs_error_incompatible_type")
})

test_that("vec_ptype2 for rando works", {
  x <- as_rando(1:10)
  expect_equal(vec_ptype(vec_ptype2(x, x)), rando())

  expect_equal(vec_ptype2(x, NA), rando())
  expect_equal(vec_ptype2(NA, x), rando())

  expect_equal(vec_ptype2(unspecified(), x), rando())
  expect_equal(vec_ptype2(x, unspecified()), rando())

  expect_error(vec_ptype2(x, 1), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2(1, x), class = "vctrs_error_incompatible_type")

  expect_error(vec_ptype2(x, ""), class = "vctrs_error_incompatible_type")
  expect_error(vec_ptype2("", x), class = "vctrs_error_incompatible_type")

  expect_error(
    vec_ptype2(data.frame(), x),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(x, data.frame()),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("vec_ptype_abbr.rando", {
  expect_equal(vec_ptype_abbr(as_rando(1:10)), "vctrs_rn")
  expect_equal(vec_ptype_full(as_rando(1:10)), "vctrs_rando")
})

test_that("proxy and data", {
  x <- rando(10)

  expect_identical(vec_ptype(vec_proxy(x)), x[0])
  expect_identical(vec_data(x), x@.Data)

  expect_false(isS4(vec_data(x)))

  expect_s4_class(vec_restore(vec_data(x), x), "vctrs_rando")
  expect_true(isS4(vec_restore(vec_data(x), x)))
})

test_that("as_not_s4() copies and works", {
  # TODO!: Translate to `vec_data()` tests

  # Initial condition
  x <- rando()
  expect_true(isS4(x))

  # Unsetting has no side effect on x
  as_not_s4(x)
  expect_true(isS4(x))

  # Unsetting actually works
  y <- as_not_s4(x)
  expect_false(isS4(y))
})
