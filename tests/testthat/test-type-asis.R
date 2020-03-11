# ------------------------------------------------------------------------------
# Printing

test_that("I() wraps contents", {
  f <- factor()

  expect_equal(vec_ptype_abbr(I(f)), "I<fct>")
  expect_equal(vec_ptype_full(I(f)), "I<factor<>>")
})

test_that("AsIs class stripped from I()", {
  df <- data.frame(x = 1, y = 1:2)
  class(df) <- c("myclass", "data.frame")

  expect_equal(vec_ptype_full(I(df)), "I<myclass<\n  x: double\n  y: integer\n>>")
  expect_equal(vec_ptype_full(I(df[1])), "I<myclass<x:double>>")
  expect_equal(vec_ptype_full(I(df[0])), "I<myclass<>>")
})

# ------------------------------------------------------------------------------
# Proxy / restore

test_that("can slice AsIs class", {
  df <- data.frame(x = I(1:3), y = I(list(4, 5, 6)))
  expect_identical(vec_slice(df, 2:3), unrownames(df[2:3, ]))
})
