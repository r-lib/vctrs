context("test-partial-frame")

test_that("has ok print method", {
  pf <- vec_type2(partial_frame(x = 1L), data.frame(y = 2))
  expect_known_output(
    print(pf),
    test_path("test-partial-frame-print.txt")
  )

  expect_equal(vec_ptype_abbr(pf), "prtl")
})

test_that("order of variables comes from data", {
  pf <- partial_frame(y = 1, x = 2)
  df <- data.frame(x = 1, y = 2)

  expect_named(vec_type_common(pf, df), c("x", "y"))
  expect_named(vec_type_common(df, pf), c("x", "y"))
})

test_that("partial variables added to end if not in data", {
  pf <- partial_frame(y = 1)
  df <- data.frame(x = 1)
  expect_named(vec_type_common(pf, df), c("x", "y"))
  expect_named(vec_type_common(df, pf), c("x", "y"))
})
