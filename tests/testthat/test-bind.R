context("test-bind")

# rows --------------------------------------------------------------------

test_that("empty inputs return an empty data frame", {
  expect_equal(vec_rbind(), data_frame())
  expect_equal(vec_rbind(NULL, NULL), data_frame())
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_rbind(df, NULL), df)
})

test_that("output is tibble if any input is tibble", {
  df <- data_frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_rbind(dt), "tbl_df")
  expect_s3_class(vec_rbind(dt, df), "tbl_df")
  expect_s3_class(vec_rbind(df, dt), "tbl_df")
})

test_that("type of column is common type of individual columns", {
  x_int <- data_frame(x = 1L)
  x_dbl <- data_frame(x = 2.5)
  x_chr <- data_frame(x = "a")

  expect_equal(vec_rbind(x_int, x_int), data_frame(x = c(1L, 1L)))
  expect_equal(vec_rbind(x_int, x_dbl), data_frame(x = c(1, 2.5)))

  expect_error(vec_rbind(x_int, x_chr), class = "vctrs_error_incompatible_type")
})

test_that("result contains union of columns", {
  expect_named(
    vec_rbind(
      data_frame(x = 1),
      data_frame(y = 1)
    ),
    c("x" , "y")
  )

  expect_named(
    vec_rbind(
      data_frame(y = 1, x = 1),
      data_frame(y = 1, z = 2)
    ),
    c("y", "x", "z")
  )
})

test_that("all inputs coerced to data frames", {
  expect_equal(
    vec_rbind(data_frame(x = 1L), c(x = 1.5)),
    data_frame(x = c(1, 1.5))
  )
})

test_that("names are supplied if needed", {
  expect_message(out <- vec_rbind(data_frame(...1 = 1), 1), "->")
  expect_equal(out, data_frame(...1 = c(1, 1)))
})

test_that("matrix becomes data frame and has names properly repaired", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vec_rbind(x), data.frame(...1 = 1:2, ...2 = 3:4))
})

test_that("can bind data.frame columns", {
  df <- data.frame(x = NA, y = 1:2)
  df$x <- data.frame(a = 1:2)

  expected <- data.frame(x = NA, y = c(1:2, 1:2))
  expected$x <- data.frame(a = c(1:2, 1:2))

  expect_equal(vec_rbind(df, df), expected)
})

test_that("can rbind unspecified vectors", {
  df <- data.frame(x = 1)
  expect_identical(vec_rbind(NA, df), data.frame(x = c(NA, 1)))
  expect_identical(vec_rbind(df, NA), data.frame(x = c(1, NA)))
  expect_identical(vec_rbind(NA, df, NA), data.frame(x = c(NA, 1, NA)))
  expect_identical(vec_rbind(c(x = NA), data.frame(x = 1)), data.frame(x = c(NA, 1)))
  expect_identical(vec_rbind(c(y = NA), df), data.frame(y = c(NA, NA), x = c(NA, 1)))

  out <- suppressMessages(vec_rbind(c(x = NA, x = NA), df))
  exp <- data.frame(x...1 = c(NA, NA), x...2 = c(NA, NA), x = c(NA, 1))
  expect_identical(out, exp)
})

test_that("as_df_row() tidies the names of unspecified vectors", {
  expect_identical(as_df_row(c(NA, NA)), c(NA, NA))
  expect_identical(as_df_row(unspecified(2)), unspecified(2))
  expect_identical(as_df_row(c(a = NA, a = NA), quiet = TRUE), data.frame(a...1 = NA, a...2 = NA))
  expect_identical(as_df_row(c(a = TRUE, a = TRUE), quiet = TRUE), data.frame(a...1 = TRUE, a...2 = TRUE))
})

test_that("can rbind spliced lists", {
  data <- list(c(a = 1, b = 2), c(a = TRUE, b = FALSE))
  expect_identical(vec_rbind(!!!data), data_frame(a = c(1, 1), b = c(2, 0)))
})

test_that("can rbind list columns", {
  out <- vec_rbind(data_frame(x = list(1, 2)), data_frame(x = list(3)))
  expect_identical(out, data_frame(x = list(1, 2, 3)))
})

test_that("can rbind missing vectors", {
  expect_identical(vec_rbind(na_int), data_frame(...1 = na_int))
  expect_identical(vec_rbind(na_int, na_int), data_frame(...1 = int(na_int, na_int)))
})

test_that("can rbind unspecified vectors", {
  expect_identical(vec_rbind(NA), data_frame(...1 = NA))
  expect_identical(vec_rbind(NA, NA), data_frame(...1 = lgl(NA, NA)))
})

test_that("vec_rbind() respects size invariants (#286)", {
  expect_identical(vec_rbind(), new_data_frame(n = 0L))

  expect_identical(vec_rbind(int(), int()), new_data_frame(n = 2L))
  expect_identical(vec_rbind(int(), TRUE), new_data_frame(list(...1 = lgl(NA, TRUE))))

  expect_identical(vec_rbind(int(), new_data_frame(n = 2L), int()), new_data_frame(n = 4L))
})

test_that("can repair names in `vec_rbind()` (#229)", {
  expect_error(vec_rbind(.name_repair = "none"), "can't be `\"none\"`")
  expect_error(vec_rbind(.name_repair = "minimal"), "can't be `\"minimal\"`")

  expect_named(vec_rbind(list(a = 1, a = 2), .name_repair = "unique"), c("a...1", "a...2"))
  expect_error(vec_rbind(list(a = 1, a = 2), .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")

  expect_named(vec_rbind(list(`_` = 1)), "_")
  expect_named(vec_rbind(list(`_` = 1), .name_repair = "universal"), c("._"))
})

test_that("vec_rbind() fails with arrays of dimensionality > 3", {
  expect_error(vec_rbind(array(NA, c(1, 1, 1))), "Can't bind arrays")
})


# cols --------------------------------------------------------------------

test_that("empty inputs give data frame", {
  expect_equal(vec_cbind(), data_frame())
  expect_equal(vec_cbind(NULL), data_frame())
  expect_equal(vec_cbind(data.frame(a = 1), NULL), data_frame(a = 1))
})

test_that("NULL is idempotent", {
  df <- data_frame(x = 1)
  expect_equal(vec_cbind(df, NULL), df)
})

test_that("outer names are respected", {
  expect_named(vec_cbind(x = 1, y = 4), c("x", "y"))
  expect_named(vec_cbind(a = data.frame(x = 1)), "a")
})

test_that("inner names are respected", {
  expect_named(vec_cbind(data_frame(x = 1), data_frame(y = 1)), c("x", "y"))
})

test_that("nameless vectors get tidy defaults", {
  expect_named(vec_cbind(1:2, 1), c("...1", "...2"))
})

test_that("matrix becomes data frame", {
  x <- matrix(1:4, nrow = 2)
  expect_equal(vec_cbind(x), data.frame(...1 = 1:2, ...2 = 3:4))

  # Packed if named
  expect_equal(vec_cbind(x = x), data_frame(x = x))
})

test_that("duplicate names are de-deduplicated", {
  expect_message(
    expect_named(vec_cbind(x = 1, x = 1), c("x...1", "x...2")),
    "x -> x...1",
    fixed = TRUE
  )
  expect_named(vec_cbind(data.frame(x = 1), data.frame(x = 1)), c("x...1", "x...2"))
})

test_that("rows recycled to longest", {
  df <- data.frame(x = 1:3)

  expect_dim(vec_cbind(df), c(3, 1))
  expect_dim(vec_cbind(df, NULL), c(3, 1))
  expect_dim(vec_cbind(df, y = 1), c(3, 2))
  expect_dim(vec_cbind(data.frame(x = 1), y = 1:3), c(3, 2))

  expect_dim(
    vec_cbind(
      data.frame(a = 1, b = 2),
      y = 1:3
    ),
    c(3, 3)
  )
})

test_that("output is tibble if any input is tibble", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(y = 2)

  expect_s3_class(vec_cbind(dt), "tbl_df")
  expect_s3_class(vec_cbind(df, dt), "tbl_df")
  expect_s3_class(vec_cbind(dt, df), "tbl_df")
})

test_that("can override default .nrow", {
  expect_dim(vec_cbind(1, .size = 3), c(3, 1))
})

test_that("can repair names in `vec_cbind()` (#227)", {
  expect_error(vec_cbind(a = 1, a = 2, .name_repair = "none"), "can't be `\"none\"`")

  expect_named(vec_cbind(a = 1, a = 2, .name_repair = "unique"), c("a...1", "a...2"))
  expect_error(vec_cbind(a = 1, a = 2, .name_repair = "check_unique"), class = "vctrs_error_names_must_be_unique")

  expect_named(vec_cbind(`_` = 1, .name_repair = "universal"), "._")

  expect_named(vec_cbind(a = 1, a = 2, .name_repair = "minimal"), c("a", "a"))
})

test_that("can supply `.names_to` to `vec_rbind()` (#229)", {
  expect_error(vec_rbind(.names_to = letters), "must be")
  expect_error(vec_rbind(.names_to = 10), "must be")

  x <- data_frame(foo = 1:2, bar = 3:4)
  y <- data_frame(foo = 5L, bar = 6L)

  expect_identical(
    vec_rbind(a = x, b = y, .names_to = "quux"),
    data_frame(foo = c(1L, 2L, 5L), bar = c(3L, 4L, 6L), quux = c("a", "a", "b"))
  )
  expect_identical(
    vec_rbind(a = x, b = y, .names_to = "foo"),
    data_frame(foo = c("a", "a", "b"), bar = c(3L, 4L, 6L))
  )

  # No names
  expect_identical(
    vec_rbind(x, y, .names_to = "quux"),
    data_frame(foo = c(1L, 2L, 5L), bar = c(3L, 4L, 6L), quux = c(1L, 1L, 2L))
  )
  expect_identical(
    vec_rbind(x, y, .names_to = "foo"),
    data_frame(foo = c(1L, 1L, 2L), bar = c(3L, 4L, 6L))
  )

  # Partial names
  expect_identical(vec_rbind(x, b = y, .names_to = "quux")$quux, c("", "", "b"))
})

test_that("vec_cbind() returns visibly (#452)", {
  # Shouldn't be needed once `check_unique` is implemented in C
  expect_visible(vctrs::vec_cbind(x = 1, .name_repair = "check_unique"))
})

test_that("vec_cbind() packs named data frames (#446)", {
  expect_identical(vec_cbind(data_frame(y = 1:3)), data_frame(y = 1:3))
  expect_identical(vec_cbind(x = data_frame(y = 1:3)), data_frame(x = data_frame(y = 1:3)))
})

test_that("vec_cbind() packs 1d arrays", {
  a <- array(1:2)
  expect_identical(vec_cbind(a), data_frame(...1 = 1:2))
  expect_identical(vec_cbind(x = a), data_frame(x = a))
})

test_that("vec_cbind() packs named matrices", {
  m <- matrix(1:4, 2)
  expect_identical(vec_cbind(m), data_frame(...1 = 1:2, ...2 = 3:4))
  expect_identical(vec_cbind(x = m), data_frame(x = m))
})

test_that("vec_cbind() never packs named vectors", {
  expect_identical(vec_cbind(1:2), data_frame(...1 = 1:2))
  expect_identical(vec_cbind(x = 1:2), data_frame(x = 1:2))
})

test_that("names are repaired late if unpacked", {
  out1 <- vec_cbind(a = 1, data_frame(b = 2, b = 3))
  out2 <- vec_cbind(a = 1, as.matrix(data_frame(b = 2, b = 3)))
  out3 <- vec_cbind(a = 1, matrix(1:2, nrow = 1))
  expect_named(out1, c("a", "b...2", "b...3"))
  expect_named(out2, c("a", "b...2", "b...3"))
  expect_named(out3, c("a", "...2", "...3"))
})

test_that("names are not repaired if packed", {
  out1 <- vec_cbind(a = 1, packed = data_frame(b = 2, b = 3))
  out2 <- vec_cbind(a = 1, packed = as.matrix(data_frame(b = 2, b = 3)))
  out3 <- vec_cbind(a = 1, packed = matrix(1:2, nrow = 1))

  expect_named(out1, c("a", "packed"))
  expect_named(out2, c("a", "packed"))
  expect_named(out3, c("a", "packed"))

  expect_named(out1$packed, c("b", "b"))
  expect_identical(colnames(out2$packed), c("b", "b"))
  expect_identical(colnames(out3$packed), NULL)
})

test_that("vec_cbind() fails with arrays of dimensionality > 3", {
  a <- array(NA, c(1, 1, 1))
  expect_error(vec_cbind(a), "Can't bind arrays")
  expect_error(vec_cbind(x = a), "Can't bind arrays")
})
