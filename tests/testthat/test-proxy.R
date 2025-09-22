test_that("vec_data() preserves names (#245)", {
  x <- set_names(letters, LETTERS)
  expect_identical(vec_names(x), vec_names(vec_data(x)))

  x <- diag(2)
  rownames(x) <- letters[1:2]
  colnames(x) <- LETTERS[1:2]
  expect_identical(vec_names(x), vec_names(vec_data(x)))
})

test_that("vec_data() preserves size (#245)", {
  x <- set_names(letters, LETTERS)
  expect_identical(vec_size(x), vec_size(vec_data(x)))

  x <- diag(2)
  expect_identical(vec_size(x), vec_size(vec_data(x)))
})

test_that("vec_data() preserves dim and dimnames (#245)", {
  x <- set_names(letters, LETTERS)
  expect_identical(vec_dim(x), vec_dim(vec_data(x)))

  x <- diag(2)
  expect_identical(vec_dim(x), vec_dim(vec_data(x)))

  x <- diag(2)
  rownames(x) <- letters[1:2]
  colnames(x) <- LETTERS[1:2]
  expect_identical(dimnames(x), dimnames(vec_data(x)))
})

test_that("strips vector attributes apart from names, dim and dimnames", {
  x <- new_vctr(1:10, a = 1, b = 2)
  expect_null(attributes(vec_data(x)))

  x <- new_vctr(c(x = 1, y = 2), a = 1, b = 2)
  expect_equal(names(attributes(vec_data(x))), "names")

  x <- new_vctr(1, a = 1, dim = c(1L, 1L))
  expect_equal(names(attributes(vec_data(x))), "dim")

  x <- new_vctr(1, a = 1, dim = c(1L, 1L), dimnames = list("foo", "bar"))
  expect_equal(names(attributes(vec_data(x))), c("dim", "dimnames"))
})

test_that("vec_proxy() is a no-op with data vectors", {
  for (x in vectors) {
    expect_identical(vec_proxy(!!x), !!x)
  }

  x <- structure(1:3, foo = "bar")
  expect_identical(vec_proxy(!!x), !!x)
})

test_that("vec_proxy() transforms records to data frames", {
  for (x in records) {
    expect_identical(vec_proxy(x), new_data_frame(unclass(x)))
  }
})

test_that("vec_proxy() is a no-op with non vectors", {
  x <- foobar(list())
  expect_identical(vec_proxy(x), x)
})

test_that("can take the proxy of non-vector objects", {
  local_env_proxy()
  expect_identical(vec_proxy(new_proxy(1:3)), 1:3)
})

test_that("vec_data() asserts vectorness", {
  expect_error(vec_data(new_sclr()), class = "vctrs_error_scalar_type")
  expect_error(vec_data(~foo), class = "vctrs_error_scalar_type")
})

test_that("vec_data() is proxied", {
  local_env_proxy()
  x <- new_proxy(mtcars)
  expect_identical(vec_data(x), vec_data(mtcars))
})

test_that("vec_proxy_equal() is recursive over data frames (#641)", {
  x <- new_data_frame(list(x = foobar(1:3), y = 41:43))
  default <- vec_proxy_equal(x)
  expect_s3_class(default$x, "vctrs_foobar")

  local_methods(vec_proxy_equal.vctrs_foobar = function(...) c(0, 0, 0))
  overridden <- vec_proxy_equal(x)
  expect_identical(overridden$x, c(0, 0, 0))
})

test_that("vec_proxy_equal() returns a POSIXct for POSIXlt objects (#901)", {
  x <- as.POSIXlt(new_date(0), tz = "UTC")
  expect_s3_class(vec_proxy_equal(x), "POSIXct")
})

test_that("vec_proxy_equal() defaults to vec_proxy() and vec_proxy_compare() defaults to vec_proxy_equal() (#1140)", {
  foobar_proxy <- function(x, ...) data_frame(x = unclass(x), y = seq_along(x))

  local_methods(vec_proxy.vctrs_foobar = foobar_proxy)

  x <- foobar(3:1)
  expect_identical(vec_proxy(x), foobar_proxy(x))
  expect_identical(vec_proxy_equal(x), foobar_proxy(x))
  expect_identical(vec_proxy_compare(x), foobar_proxy(x))

  local_methods(vec_proxy_equal.vctrs_foobar = function(x, ...) {
    foobar_proxy(letters[x])
  })

  expect_identical(vec_proxy_equal(x), data_frame(x = letters[3:1], y = 1:3))
  expect_identical(vec_proxy_compare(x), data_frame(x = letters[3:1], y = 1:3))
})

test_that("equal/compare/order proxy methods that return data frames are automatically flattened", {
  x <- new_vctr(1:2, class = "custom")

  equal <- data_frame(a = 1:2, b = 3:4)
  order <- data_frame(a = 3:4, b = 4:5)

  local_methods(
    vec_proxy_equal.custom = function(x, ...) data_frame(col = equal),
    vec_proxy_order.custom = function(x, ...) data_frame(col = order)
  )

  expect_identical(vec_proxy_equal(x), equal)
  expect_identical(vec_proxy_compare(x), equal)
  expect_identical(vec_proxy_order(x), order)
})

test_that("equal/compare/order proxy methods that return 1 column data frames are automatically unwrapped", {
  x <- new_vctr(1:2, class = "custom")

  equal <- 1:2
  order <- 3:4

  local_methods(
    vec_proxy_equal.custom = function(x, ...) data_frame(a = equal),
    vec_proxy_order.custom = function(x, ...) {
      data_frame(col = data_frame(a = order))
    }
  )

  expect_identical(vec_proxy_equal(x), equal)
  expect_identical(vec_proxy_compare(x), equal)
  expect_identical(vec_proxy_order(x), order)
})

test_that("vec_data() preserves data frames", {
  expect_identical(
    vec_data(tibble(x = 1)),
    data_frame(x = 1)
  )

  # Rownames are preserved
  expect_identical(
    vec_data(mtcars),
    mtcars
  )
})
