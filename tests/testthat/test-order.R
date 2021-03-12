test_that("can request NAs sorted first", {
  expect_equal(vec_order(c(1, NA), "asc", "largest"), 1:2)
  expect_equal(vec_order(c(1, NA), "desc", "largest"), 2:1)

  expect_equal(vec_order(c(1, NA), "asc", "smallest"), 2:1)
  expect_equal(vec_order(c(1, NA), "desc", "smallest"), 1:2)
})

test_that("can order complex vectors", {
  x <- complex(real = c(1, 2, 2, 3, 3), imaginary = c(5, 4, 3, 2, NA))

  expect_equal(vec_order(x, direction = "asc", na_value = "largest"), c(1, 3, 2, 4, 5))
  expect_equal(vec_order(x, direction = "desc", na_value = "largest"), rev(c(1, 3, 2, 4, 5)))
  expect_equal(vec_order(x, direction = "asc", na_value = "smallest"), c(5, 1, 3, 2, 4))
  expect_equal(vec_order(x, direction = "desc", na_value = "smallest"), rev(c(5, 1, 3, 2, 4)))
})

test_that("can sort data frames", {
  df <- data.frame(x = c(1, 2, 1), y = c(1, 2, 2))

  out1 <- vec_sort(df)
  expect_equal(out1, data.frame(x = c(1, 1, 2), y = c(1, 2, 2)))

  out2 <- vec_sort(df, "desc")
  expect_equal(out2, data.frame(x = c(2, 1, 1), y = c(2, 2, 1)))
})

test_that("can sort empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_sort(df1), df1)

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_sort(df2), df2)
})

test_that("can order tibbles that contain non-comparable objects", {
  expect_equal(vec_order(data_frame(x = list(10, 2, 1))), 1:3)
})

test_that("can order matrices and arrays (#306)", {
  x <- matrix(c(1, 1, 1, 1, 2, 1), ncol = 2)
  expect_identical(vec_order(x), c(1L, 3L, 2L))

  x <- array(1:8, c(2, 2, 2))
  x[2] <- 1
  x[3] <- 5
  expect_identical(vec_order(x), 2:1)
})

test_that("can order empty data frames (#356)", {
  df1 <- data.frame()
  expect_equal(vec_order(df1), integer())

  df2 <- data.frame(x = numeric(), y = integer())
  expect_equal(vec_order(df2), integer())
})

test_that("can order data frames with data frame columns (#527)", {
  expect_equal(
    vec_order(iris),
    vec_order(data_frame(iris = iris))
  )
})

test_that("can order data frames (and subclasses) with matrix columns", {
  df <- new_data_frame(n = 2L)

  df$x <- new_data_frame(list(y = matrix(1:2, 2)))
  expect_identical(vec_order(df), 1:2)

  df$x <- tibble::tibble(y = matrix(1:2, 2))
  expect_identical(vec_order(df), 1:2)
})

test_that("classed proxies do not affect performance (tidyverse/dplyr#5423)", {
  skip_on_cran()
  x <- glue::glue("{1:10000}")
  expect_time_lt(vec_order(x), 0.2)
})

test_that("can order data frames that don't allow removing the column names (#1298)", {
  skip_if_not_installed("withr")

  local_methods(
    `names<-.vctrs_foobar` = function(x, value) {
      if (is.null(value)) {
        abort("Cannot remove names.")
      }
      NextMethod()
    }
  )

  df <- foobar(data.frame(x = 1, y = 2))

  expect_silent(expect_identical(vec_order(df), 1L))
})
