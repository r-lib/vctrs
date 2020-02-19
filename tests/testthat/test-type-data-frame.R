context("test-type-data-frame")

# printing ----------------------------------------------------------------

test_that("data frames print nicely", {
  expect_equal(vec_ptype_abbr(mtcars), "df[,11]")

  verify_output(test_path("test-type-data-frame.txt"), {
    vec_ptype_show(mtcars)
    vec_ptype_show(iris)
  })
})

test_that("embedded data frames print nicely", {
  df <- data.frame(x = 1:3)
  df$a <- data.frame(a = 1:3, b = letters[1:3])
  df$b <- list_of(1, 2, 3)
  df$c <- as_list_of(split(data.frame(x = 1:3, y = letters[1:3]), 1:3))

  verify_output(test_path("test-type-data-frame-embedded.txt"), {
    vec_ptype_show(df)
  })
})

# coercing ----------------------------------------------------------------

test_that("data frame only combines with other data frames or NULL", {
  dt <- data.frame(x = 1)
  expect_equal(vec_ptype_common(dt, NULL), vec_ptype(dt))
  expect_error(vec_ptype_common(dt, 1:10), class = "vctrs_error_incompatible_type")
})

test_that("data frame takes max of individual variables", {
  dt1 <- data.frame(x = FALSE, y = 1L)
  dt2 <- data.frame(x = 1.5, y = 1.5)

  expect_equal(vec_ptype_common(dt1, dt2), vec_ptype_common(dt2))
})

test_that("data frame combines variables", {
  dt1 <- data.frame(x = 1)
  dt2 <- data.frame(y = 1)

  dt3 <- max(dt1, dt2)
  expect_equal(
    vec_ptype_common(dt1, dt2),
    vec_ptype_common(data.frame(x = double(), y = double()))
  )
})

test_that("empty data frame still has names", {
  df <- data.frame()
  out <- vec_ptype_common(df, df)

  expect_equal(names(out), character())
})

# casting -----------------------------------------------------------------

test_that("safe casts work as expected", {
  df <- data.frame(x = 1, y = 0)

  expect_equal(vec_cast(NULL, df), NULL)
  expect_equal(vec_cast(df, df), df)

  expect_equal(vec_cast(data.frame(x = TRUE, y = FALSE), df), df)
})

test_that("warn about lossy coercions", {
  df1 <- data.frame(x = 1, y = 1)
  df2 <- data.frame(x = c("a", 1), stringsAsFactors = FALSE)

  expect_lossy(vec_cast(df1, df1[1]), df1[1], x = df1, to = df1[1])
  expect_lossy(vec_cast(df2, df1), data.frame(x = dbl(NA, 1), y = dbl(NA, NA)), x = chr(), to = dbl())

  out <-
    allow_lossy_cast(
      allow_lossy_cast(
        vec_cast(df2, df1),
        chr(), dbl()
      ),
      df2, df1
    )

  expect_identical(out, data.frame(x = dbl(NA, 1), y = dbl(NA, NA)))
})

test_that("invalid cast generates error", {
  expect_error(vec_cast(1L, data.frame()), class = "vctrs_error_incompatible_cast")
})

test_that("column order matches type", {
  df1 <- data.frame(x = 1, y = "a")
  df2 <- data.frame(x = TRUE, z = 3)

  df3 <- vec_cast(df2, vec_ptype_common(df1, df2))
  expect_named(df3, c("x", "y", "z"))
})

test_that("casts preserve outer class", {
  df <- data.frame(x = 1)
  dt <- tibble::tibble(x = 1)

  expect_s3_class(vec_cast(df, dt), "tbl_df")
  expect_s3_class(vec_cast(dt, df), "data.frame")
})

test_that("restore generates correct row/col names", {
  df1 <- data.frame(x = NA, y = 1:4, z = 1:4)
  df1$x <- data.frame(a = 1:4, b = 1:4)

  df2 <- vec_restore(lapply(df1[1:3], vec_slice, 1:2), df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -2)
})

test_that("restore keeps automatic row/col names", {
  df1 <- data.frame(x = NA, y = 1:4, z = 1:4)
  df1$x <- data.frame(a = 1:4, b = 1:4)

  df2 <- vec_restore(df1, df1)

  expect_named(df2, c("x", "y", "z"))
  expect_equal(.row_names_info(df2), -4)
})

test_that("cast to empty data frame preserves number of rows", {
  out <- vec_cast(new_data_frame(n = 10L), new_data_frame())
  expect_equal(nrow(out), 10L)
})

test_that("can cast unspecified to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_identical(vec_cast(unspecified(3), df), vec_init(df, 3))
})

test_that("can cast list of data frames to data frame", {
  df <- data.frame(x = 1, y = 2L)
  expect_equal(vec_cast(list(df, df), df), vec_slice(df, c(1, 1)))
})

test_that("can only cast list of data frames to data frame if they are all size 1", {
  df <- data.frame(x = 1:2)
  expect_error(vec_cast(list(df), df), class = "vctrs_error_cast_lossy")
})

test_that("can restore lists with empty names", {
  expect_identical(vec_restore(list(), data.frame()), data.frame())
})

test_that("can restore subclasses of data frames", {
  expect_identical(vec_restore(list(), subclass(data.frame())), subclass(data.frame()))
  local_methods(
    vec_restore.vctrs_foobar = function(x, to, ..., i) "dispatched"
  )
  expect_identical(vec_restore(list(), subclass(data.frame())), "dispatched")
})

test_that("df_as_dataframe() checks for names", {
  x <- data_frame(1)
  y <- data_frame(2)
  expect_error(vec_cast_common(x, y), "must have names")
})

test_that("can slice AsIs class", {
  df <- data.frame(x = I(1:3), y = I(list(4, 5, 6)))
  expect_identical(vec_slice(df, 2:3), unrownames(df[2:3, ]))
})

# new_data_frame ----------------------------------------------------------

test_that("can construct an empty data frame", {
  expect_identical(new_data_frame(), data.frame())
})

test_that("can validly set the number of rows when there are no columns", {
  expect <- structure(
    list(),
    class = "data.frame",
    row.names = .set_row_names(2L),
    names = character()
  )

  expect_identical(new_data_frame(n = 2L), expect)
})

test_that("can add additional classes", {
  expect_s3_class(new_data_frame(class = "foobar"), "foobar")
  expect_s3_class(new_data_frame(class = c("foo", "bar")), c("foo", "bar"))
})

test_that("can add additional attributes", {
  expect <- data.frame()
  attr(expect, "foo") <- "bar"
  attr(expect, "a") <- "b"

  expect_identical(new_data_frame(foo = "bar", a = "b"), expect)
})

test_that("size is pulled from first column if not supplied", {
  x <- new_data_frame(list(x = 1:5, y = 1:6))
  expect_identical(.row_names_info(x, type = 1), -5L)
})

test_that("can construct a data frame without column names", {
  expect_named(new_data_frame(list(1, 2)), NULL)
})

test_that("the names on an empty data frame are an empty character vector", {
  expect_identical(names(new_data_frame()), character())
})

test_that("attributes with special names are ignored", {
  expect_identical(
    names(new_data_frame(list(), 0L, names = "name")),
    character()
  )

  expect_identical(
    attr(new_data_frame(list(), 0L, row.names = "rowname"), "row.names"),
    integer()
  )
})

test_that("`x` must be a list", {
  expect_error(new_data_frame(1), "`x` must be a list")
})

test_that("if supplied, `n` must be an integer of size 1", {
  expect_error(new_data_frame(n = c(1L, 2L)), "must be an integer of size 1")
  expect_error(new_data_frame(n = "x"), "must be an integer of size 1")
})

test_that("`class` must be a character vector", {
  expect_error(new_data_frame(class = 1), "must be NULL or a character vector")
})

test_that("flat width is computed", {
  df_flat_width <- function(x) {
    .Call(vctrs_df_flat_width, x)
  }
  expect_identical(df_flat_width(mtcars), ncol(mtcars))

  df <- tibble(x = 1, y = tibble(x = 2, y = tibble(x = 3), z = 4), z = 5)
  expect_identical(df_flat_width(df), 5L)
})
