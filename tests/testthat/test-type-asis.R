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

  expect_equal(
    vec_ptype_full(I(df)),
    "I<myclass<\n  x: double\n  y: integer\n>>"
  )
  expect_equal(vec_ptype_full(I(df[1])), "I<myclass<x:double>>")
  expect_equal(vec_ptype_full(I(df[0])), "I<myclass<>>")
})

# ------------------------------------------------------------------------------
# Proxy / restore

test_that("can slice AsIs class", {
  df <- data.frame(x = I(1:3), y = I(list(4, 5, 6)))
  expect_identical(vec_slice(df, 2:3), unrownames(df[2:3, ]))
})

test_that("equality proxy is forwarded correctly for atomic types (#1557)", {
  # We don't define any equality proxies for base atomic types, but we can fake it
  local_methods(vec_proxy_equal.integer = function(x, ...) "dispatched")
  asis <- I(1L)
  expect_identical(vec_proxy_equal(asis), "dispatched")
})

test_that("comparison proxy is forwarded correctly for atomic types (#1557)", {
  # vec_proxy_compare.raw() exists
  x <- raw()
  asis <- I(x)

  expect_identical(vec_proxy_compare(asis), vec_proxy_compare(x))
  expect_identical(vec_proxy_compare(asis), integer())
})

test_that("order proxy is forwarded correctly for atomic types (#1557)", {
  # vec_proxy_order.list() exists
  x <- list(2, 1, 2)
  asis <- I(x)

  expect_identical(vec_proxy_order(asis), vec_proxy_order(x))
  expect_identical(vec_proxy_order(asis), c(1L, 2L, 1L))
})

test_that("proxy is restored correctly for non-atomic types (#1903)", {
  x <- new_rcrd(list(x = 1:3, y = 2:4), class = "test")
  x_ptype <- new_rcrd(list(x = integer(), y = integer()), class = "test")
  asis <- I(x)
  asis_ptype <- I(x_ptype)

  expect_identical(vec_restore(vec_proxy(asis), asis_ptype), I(vec_restore(vec_proxy(x), x_ptype)))
  expect_identical(vec_restore(vec_proxy(asis), asis_ptype), asis)
})

# ------------------------------------------------------------------------------
# Coercion

test_that("can take the common type of identical AsIs objects", {
  expect_identical(vec_ptype2(I(1), I(1)), I(numeric()))
})

test_that("AsIs objects throw ptype2 errors with their underlying types", {
  expect_snapshot({
    (expect_error(
      vec_ptype2(I(1), I("x")),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("AsIs always wraps the common type", {
  expect_identical(vec_ptype2(I(1L), 1), I(numeric()))
  expect_identical(vec_ptype2(1, I(1L)), I(numeric()))
})

# ------------------------------------------------------------------------------
# Casting

test_that("can cast one AsIs to another AsIs", {
  expect_identical(vec_cast(I(1), I(1)), I(1))
  expect_identical(vec_cast(I(1), I(1L)), I(1L))
})

test_that("AsIs objects throw cast errors with their underlying types", {
  expect_snapshot({
    (expect_error(
      vec_cast(I(1), I(factor("x"))),
      class = "vctrs_error_incompatible_type"
    ))
  })
})

test_that("casting from an AsIs drops the AsIs class", {
  expect_identical(vec_cast(I(1), 1), 1)
})

test_that("casting to an AsIs adds the AsIs class", {
  expect_identical(vec_cast(1, I(1)), I(1))
})

# ------------------------------------------------------------------------------
# Misc

test_that("can `vec_c()` with only AsIs objects", {
  expect_identical(vec_c(I(1), I(2)), I(c(1, 2)))
  expect_identical(vec_c(I(1), I(2L)), I(c(1, 2)))
})

test_that("can `vec_c()` with AsIs objects mixed with other types", {
  expect_identical(vec_c(I(1L), 1), I(c(1, 1)))
})
