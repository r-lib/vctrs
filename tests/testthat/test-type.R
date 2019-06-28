context("test-type")


test_that("vec_ptype() is a no-op for non-vectors", {
  expect_null(vec_ptype(NULL))
  expect_identical(vec_ptype(quote(name)), quote(name))
  expect_identical(vec_ptype(partial_frame(x = 1)), partial_frame(x = 1))
})

test_that(".ptype argument overrides others", {
  expect_equal(vec_ptype_common(.ptype = 1:10), numeric())
})

test_that(".ptype required in strict mode", {
  old <- options(vctrs.no_guessing = TRUE)
  on.exit(options(old))

  expect_error(vec_ptype_common(), "strict mode")
})

test_that("can feed ptype into itself", {
  expect_equal(vec_ptype_common(vec_ptype_common(1:10)), numeric())
})

test_that("finalised prototypes created from under specified inputs", {
  expect_equal(vec_ptype_common(), NULL)
  expect_equal(vec_ptype_common(NULL), NULL)

  expect_equal(vec_ptype_common(NA), logical())
  expect_equal(vec_ptype_common(NA, NULL), logical())
  expect_equal(vec_ptype_common(NULL, NA), logical())
})

test_that("finalised prototypes created from under specified data frame cols", {
  df <- data.frame(x = NA)
  expect_equal(vec_ptype_common(df)$x, logical())
})

test_that("non-missing logical get correct type", {
  expect_equal(vec_ptype_common(TRUE), logical())
})

test_that("output tests", {
  expect_known_output(vec_ptype_show(), "out/vec-ptype-0.txt")
  expect_known_output(vec_ptype_show(integer()), "out/vec-ptype-1.txt")
  expect_known_output(vec_ptype_show(integer(), double()), "out/vec-ptype-2.txt")
  expect_known_output(vec_ptype_show(logical(), integer(), double()), "out/vec-ptype-3.txt")
})

test_that("vec_ptype_common() handles matrices", {
  m <- matrix(1:4, nrow = 2)
  expect_identical(vec_ptype_common(m, m), matrix(int(), ncol = 2))
})

test_that("vec_ptype_common() includes index in argument tag", {
  df1 <- tibble(x = tibble(y = tibble(z = 1)))
  df2 <- tibble(x = tibble(y = tibble(z = "a")))

  # Create a column name too large for default buffer
  nm <- strrep("foobarfoobar", 10)
  large_df1 <- set_names(df1, nm)
  large_df2 <- set_names(df2, nm)

  expect_known_output_nobang(file = test_path("test-type-vec-type-common-error.txt"), {
    try2(vec_ptype_common(df1, df2))
    try2(vec_ptype_common(df1, df1, df2))
    try2(vec_ptype_common(large_df1, large_df2))

    # Names
    try2(vec_ptype_common(foo = TRUE, bar = "foo"))
    try2(vec_ptype_common(foo = TRUE, baz = FALSE, bar = "foo"))
    try2(vec_ptype_common(foo = df1, bar = df2))
    try2(vec_ptype_common(df1, df1, bar = df2))

    # One splice box
    try2(vec_ptype_common(TRUE, !!!list(1, "foo")))
    try2(vec_ptype_common(TRUE, !!!list(1, 2), "foo"))
    try2(vec_ptype_common(1, !!!list(TRUE, FALSE), "foo"))

    # One named splice box
    try2(vec_ptype_common(foo = TRUE, !!!list(FALSE, FALSE), bar = "foo"))
    try2(vec_ptype_common(foo = TRUE, !!!list(bar = 1, "foo")))
    try2(vec_ptype_common(foo = TRUE, !!!list(bar = "foo")))
    try2(vec_ptype_common(foo = TRUE, !!!list(bar = FALSE), baz = "chr"))

    # Two splice boxes in next and current
    try2(vec_ptype_common(foo = TRUE, !!!list(bar = FALSE), !!!list(baz = "chr")))
  })
})

test_that("proxied types are have s3 bare type", {
  for (x in proxied_empty_types) {
    expect_identical(vec_typeof_bare(x), "s3")
  }
})

test_that("vec_ptype() preserves attributes of unproxied structures", {
  expect_identical(vec_ptype(foobar(dbl(1))), foobar(dbl()))

  # Here `foobar()` is treated as a scalar so is returned as is
  expect_identical(vec_ptype(foobar(list(1))), foobar(list(1)))
})

test_that("can retrieve type info", {
  exp <- list(type = "integer", proxy_method = NULL)
  expect_identical(vec_type_info(1:3), exp)

  exp <- list(type = "s3", proxy_method = NULL)
  expect_identical(vec_type_info(~foo), exp)

  exp <- list(type = "dataframe", proxy_method = vec_proxy.data.frame)
  expect_identical(vec_type_info(mtcars), exp)
})

test_that("can retrieve proxy info", {
  exp <- list(type = "integer", proxy_method = NULL, proxy = 1:3)
  expect_identical(vec_proxy_info(1:3), exp)

  exp <- list(type = "scalar", proxy_method = NULL, proxy = ~foo)
  expect_identical(vec_proxy_info(~foo), exp)

  exp <- list(type = "dataframe", proxy_method = vec_proxy.data.frame, proxy = mtcars)
  expect_identical(vec_proxy_info(mtcars), exp)
})

test_that("class_type() detects classes", {
  expect_identical(class_type(data.frame()), "bare_data_frame")
  expect_identical(class_type(tibble::tibble()), "bare_tibble")
  expect_identical(class_type(subclass(data.frame())), "data_frame")

  expect_identical(class_type(as.POSIXlt(Sys.Date())), "posixlt")
  expect_identical(class_type(new_rcrd(list(a = 1))), "rcrd")
  expect_identical(class_type(subclass(as.POSIXlt(Sys.Date()))), "posixlt")
  expect_identical(class_type(subclass(new_rcrd(list(a = 1)))), "rcrd")

  expect_identical(class_type(NA), "none")
  expect_identical(class_type(foobar()), "unknown")
})
