context("test-partial-factor")

test_that("has ok print method", {
  partial <- partial_factor("x")
  expect_known_output(
    print(partial),
    test_path("test-partial-factor-print-partial.txt")
  )

  both <- vec_ptype2(partial, factor("y"))
  expect_known_output(
    print(both),
    test_path("test-partial-factor-print-both.txt")
  )

  empty <- partial_factor()
  expect_known_output(
    print(empty),
    test_path("test-partial-factor-print-empty.txt")
  )

  learned <- vec_ptype2(empty, factor("y"))
  expect_known_output(
    print(learned),
    test_path("test-partial-factor-print-learned.txt")
  )

  expect_equal(vec_ptype_abbr(partial), "prtl_fctr")
})

test_that("order of levels comes from data", {
  pfctr <- partial_factor(c("y", "x"))
  fctr <- factor(levels = c("x", "y"))

  expect_equal(levels(vec_ptype_common(pfctr, fctr)), c("x", "y"))
  expect_equal(levels(vec_ptype_common(fctr, pfctr)), c("x", "y"))
})

test_that("partial levels added to end if not in data", {
  pfctr <- partial_factor("y")
  fctr <- factor(levels = "x")

  expect_equal(levels(vec_ptype_common(pfctr, fctr)), c("x", "y"))
  expect_equal(levels(vec_ptype_common(fctr, pfctr)), c("x", "y"))
})

test_that("can assert partial factors based on level presence", {
  pfctr <- partial_factor("y")

  expect_true(vec_is(factor("y"), pfctr))
  expect_false(vec_is(factor("x"), pfctr))

  expect_true(vec_is(factor(c("x", "y")), pfctr))

  pfctr <- partial_factor(c("y", "z"))

  expect_false(vec_is(factor("y"), pfctr))
  expect_true(vec_is(factor(c("y", "z")), pfctr))
  expect_true(vec_is(factor(c("x", "y", "z")), pfctr))
})

# TODO - why is this not working?
# test_that("can assert partial factor based on factor type", {
#   pfctr <- partial_factor()
#   expect_false(vec_is(1, pfctr))
# })
