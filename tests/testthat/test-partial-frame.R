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

test_that("can assert partial frames based on column presence", {
  pf <- partial_frame(y = 1)

  expect_true(vec_is(data.frame(y = 2), pf))
  expect_false(vec_is(data.frame(x = 1), pf))

  expect_true(vec_is(data.frame(x = 1, y = 2), pf))
  expect_true(vec_is(data.frame(x = 1, y = 2, z = 3), pf))

  pf <- partial_frame(y = 1, z = 3)

  expect_false(vec_is(data.frame(y = 2), pf))
  expect_false(vec_is(data.frame(x = 1), pf))
  expect_false(vec_is(data.frame(x = 1, y = 2), pf))
  expect_true(vec_is(data.frame(x = 1, y = 2, z = 3), pf))
})

test_that("can assert partial frames based on column type", {
  pf <- partial_frame(y = 1)
  expect_false(vec_is(data.frame(y = "2"), pf))
})

test_that("incompatible data frames are an error", {
  df <- data.frame(y = 1)
  expect_error(vec_type2(df, partial_frame(y = chr())), class = "vctrs_error_incompatible_type")
  expect_error(new_partial_frame(df, data.frame(y = chr())), class = "vctrs_error_incompatible_type")
})
