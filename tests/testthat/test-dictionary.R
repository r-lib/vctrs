context("test-dictionary")


# counting ----------------------------------------------------------------

test_that("vec_count counts number observations", {
  x <- vec_count(rep(1:3, 1:3), sort = "key")
  expect_equal(x, data.frame(key = 1:3, count = 1:3))
})

test_that("vec_count works with matrices", {
  x <- matrix(c(1, 1, 1, 2, 2, 1), c(3, 2))

  out <- vec_count(x)
  exp <- data_frame(key = c(NA, NA), count = int(2L, 1L))
  exp$key <- vec_slice(x, c(1, 3))

  expect_identical(out, exp)
})

test_that("vec_count works with arrays", {
  x <- array(c(rep(1, 3), rep(2, 3)), dim = c(3, 2, 1))
  expect <- data.frame(key = NA, count = 3)
  expect$key <- vec_slice(x, 1L)
  expect_equal(vec_count(x), expect)
})

test_that("vec_count works for zero-length input", {
  x <- vec_count(integer(), sort = "none")
  expect_equal(x, data.frame(key = integer(), count = integer()))
})


# duplicates and uniques --------------------------------------------------

test_that("vec_duplicated reports on duplicates regardless of position", {
  x <- c(1, 1, 2, 3, 4, 4)
  expect_equal(vec_duplicate_detect(x), c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("vec_duplicate_any returns single TRUE/FALSE", {
  expect_false(vec_duplicate_any(c(1:10)))
  expect_true(vec_duplicate_any(c(1:10, 1)))
})

test_that("vec_duplicate_id gives position of first found", {
  x <- c(1, 2, 3, 1, 4)
  expect_equal(vec_duplicate_id(x), c(1, 2, 3, 1, 5))
})

test_that("vec_unique matches unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique(x), unique(x))
})

test_that("vec_unique matches unique for matrices", {
  x <- matrix(c(1, 1, 2, 2), ncol = 2)
  expect_equal(vec_unique(x), unique(x))
})

test_that("vec_unique_count matches length + unique", {
  x <- sample(100, 1000, replace = TRUE)
  expect_equal(vec_unique_count(x), length(unique(x)))
})

test_that("also works for data frames", {
  df <- data.frame(x = 1:3, y = letters[3:1], stringsAsFactors = FALSE)
  idx <- c(1L, 1L, 1L, 2L, 2L, 3L)
  df2 <- df[idx, , drop = FALSE]
  rownames(df2) <- NULL

  expect_equal(vec_duplicate_detect(df2), vec_duplicate_detect(idx))
  expect_equal(vec_unique(df2), vec_slice(df, vec_unique(idx)))

  count <- vec_count(df2, sort = "key")
  expect_equal(count$key, df)
  expect_equal(count$count, vec_count(idx)$count)

  exp <- tibble(x = c(1, 1, 2), y = c(1, 2, 3))
  expect_identical(vec_unique(vec_slice(exp, c(1, 1, 2, 3))), exp)
})

test_that("vec_unique() handles matrices (#327)", {
  x <- matrix(c(1, 2, 3, 4), c(2, 2))
  y <- matrix(c(1, 2, 3, 5), c(2, 2))
  expect_identical(vec_unique(list(x, x)), list(x))
  expect_identical(vec_unique(list(x, y)), list(x, y))

  x <- matrix(c(1, 2, 1, 1, 2, 1), nrow = 3)
  expect_identical(vec_unique(x), vec_slice(x, 1:2))
})

test_that("vec_unique() works with 1D arrays", {
  # 1D arrays are dispatched to `as.data.frame.vector()` which
  # currently does not strip dimensions. This caused an infinite
  # recursion.
  expect_identical(vec_unique(array(1:2)), array(1:2))

  x <- new_vctr(c(1, 1, 1, 2, 1, 2), dim = c(3, 2))
  expect_identical(vec_unique(x), new_vctr(c(1, 1, 2, 1), dim = c(2, 2)))
})

test_that("unique functions take the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)

  expect_true(vec_in(tuple(2, 100), x))
  expect_identical(vec_match(tuple(2, 100), x), 2L)
})

test_that("vec_unique() can detect uniqueness with the same string in various encodings (#553)", {
  x <- unlist(encodings(), use.names = FALSE)

  expect_equal(vec_unique(x), x[1])
  expect_equal(vec_unique(x), unique(x))
})

test_that("vec_unique() returns differently encoded strings in the order they appear", {
  enc <- encodings()
  x <- c(enc$unknown, enc$utf8)
  y <- c(enc$utf8, enc$unknown)

  expect_equal(Encoding(vec_unique(x)), "unknown")
  expect_equal(Encoding(vec_unique(y)), "UTF-8")
})

test_that("vec_unique() can determine uniqueness when the encoding is the same", {
  encs <- c(encodings(), list(bytes = encoding_bytes()))

  for (enc in encs) {
    x <- c(enc, enc)
    expect_equal(vec_unique(x), x[1])
    expect_equal(vec_unique(x), unique(x))
  }
})

test_that("vec_unique() fails purposefully with bytes strings and other encodings", {
  for (enc in encodings()) {
    x <- c(encoding_bytes(), enc)
    expect_error(vec_unique(x), "translating strings with \"bytes\" encoding")
  }
})

# matching ----------------------------------------------------------------

test_that("vec_match() matches match()", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)

  expect_equal(vec_match(n, h), match(n, h))
})

test_that("vec_in() matches %in%", {
  n <- c(1:3, NA)
  h <- c(4, 2, 1, NA)

  expect_equal(vec_in(n, h), n %in% h)
})

test_that("vec_match works with empty data frame", {
  out <- vec_match(
    new_data_frame(n = 3L),
    new_data_frame(n = 0L)
  )
  expect_equal(out, vec_init(integer(), 3))
})

test_that("matching functions take the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)

  expect_identical(vec_unique_loc(x), 1:2)
  expect_identical(unique(x), tuple(c(1, 2), 1:2))

  expect_true(vec_duplicate_any(x))
  expect_identical(vec_duplicate_id(x), c(1L, 2L, 1L))
  expect_identical(vec_unique_count(x), 2L)

  expect_identical(vec_duplicate_detect(x), c(TRUE, FALSE, TRUE))
})

test_that("can take the unique loc of 1d arrays (#461)", {
  x <- array(c(1, 1, 2, 2, 3))
  y <- array(c(1, 1, 2, 2, 3), dimnames = list(NULL))
  expect_identical(vctrs::vec_unique_loc(x), int(1, 3, 5))
  expect_identical(vctrs::vec_unique_loc(y), int(1, 3, 5))

  z <- array(c(1, 1, 2, 2, 3, 4), c(3, 2))
  expect_silent(expect_identical(vctrs::vec_unique_loc(y), int(1, 3, 5)))
})

test_that("can use matching functions with various encoding combinations", {
  encs <- encodings()

  for (x_enc in encs) {
    for (y_enc in encs) {
      expect_equal(vec_match(x_enc, y_enc), 1L)
      expect_equal(vec_match(x_enc, y_enc), match(x_enc, y_enc))

      expect_equal(vec_in(x_enc, y_enc), TRUE)
      expect_equal(vec_in(x_enc, y_enc), x_enc %in% y_enc)
    }
  }
})

test_that("can use matching functions with strings containing only bytes", {
  bytes <- encoding_bytes()

  expect_equal(vec_match(bytes, bytes), 1L)
  expect_equal(vec_match(bytes, bytes), match(bytes, bytes))

  expect_equal(vec_in(bytes, bytes), TRUE)
  expect_equal(vec_in(bytes, bytes), bytes %in% bytes)
})

test_that("cannot use matching functions when mixing bytes with other encodings", {
  encs <- encodings()
  bytes <- encoding_bytes()

  error <- "translating strings with \"bytes\" encoding"

  for (enc in encs) {
    expect_error(vec_match(enc, bytes), error)
    expect_error(vec_match(bytes, enc), error)

    expect_error(vec_in(enc, bytes), error)
    expect_error(vec_in(bytes, enc), error)
  }
})

test_that("can use matching functions when one string has multiple encodings", {
  encs <- encodings()

  utf8_unknown <- c(encs$utf8, encs$unknown)

  expect_equal(vec_match(utf8_unknown, encs$latin1), c(1L, 1L))
  expect_equal(vec_match(utf8_unknown, encs$latin1), match(utf8_unknown, encs$latin1))
})

test_that("can use matching functions with lists of characters with different encodings", {
  encs <- encodings()

  lst_ascii <- list("ascii")
  lst_latin1 <- list(encs$latin1)
  lst_ascii_latin1 <- c(lst_ascii, lst_latin1)
  lst_utf8 <- list(encs$utf8)

  lst_of_lst_utf8 <- list(lst_utf8)
  lst_of_lst_ascii_latin1 <- list(lst_ascii, lst_latin1)

  expect_equal(vec_match(lst_ascii, lst_ascii), 1L)
  expect_equal(vec_in(lst_ascii, lst_ascii), TRUE)

  expect_equal(vec_match(lst_utf8, lst_ascii_latin1), 2L)
  expect_equal(vec_in(lst_utf8, lst_ascii_latin1), TRUE)

  expect_equal(vec_match(lst_utf8, lst_ascii_latin1), match(lst_utf8, lst_ascii_latin1))
  expect_equal(vec_in(lst_utf8, lst_ascii_latin1), lst_utf8 %in% lst_ascii_latin1)

  expect_equal(vec_match(lst_of_lst_utf8, lst_of_lst_ascii_latin1), 2L)
  expect_equal(vec_in(lst_of_lst_utf8, lst_of_lst_ascii_latin1), TRUE)

  expect_equal(vec_match(lst_of_lst_utf8, lst_of_lst_ascii_latin1), match(lst_of_lst_utf8, lst_of_lst_ascii_latin1))
  expect_equal(vec_in(lst_of_lst_utf8, lst_of_lst_ascii_latin1), lst_of_lst_utf8 %in% lst_of_lst_ascii_latin1)
})

test_that("can use matching functions with data frames with string columns", {
  encs <- encodings()

  df_utf8 <- data_frame(x = encs$utf8, y = 2)
  df_unknown <- data_frame(x = rep(encs$unknown, 2), y = c(1, 2))

  expect_equal(vec_match(df_unknown, df_unknown), 1:2)
  expect_equal(vec_in(df_unknown, df_unknown), c(TRUE, TRUE))

  expect_equal(vec_match(df_utf8, df_unknown), 2L)
  expect_equal(vec_in(df_utf8, df_unknown), TRUE)
})

test_that("can use matching functions with data frame subclasses with string columns", {
  encs <- encodings()

  df_utf8 <- new_data_frame(list(x = encs$utf8, y = 2), class = "subclass")
  df_unknown <- new_data_frame(list(x = rep(encs$unknown, 2), y = c(1, 2)), class = "subclass")

  expect_equal(vec_match(df_unknown, df_unknown), 1:2)
  expect_equal(vec_in(df_unknown, df_unknown), c(TRUE, TRUE))

  expect_equal(vec_match(df_utf8, df_unknown), 2L)
  expect_equal(vec_in(df_utf8, df_unknown), TRUE)
})

test_that("can use matching functions with lists of data frames with string columns", {
  encs <- encodings()

  df_utf8 <- data_frame(x = encs$utf8, y = 2)
  df_unknown_1 <- data_frame(x = encs$unknown, y = 1)
  df_unknown_2 <- data_frame(x = encs$unknown, y = 2)

  lst_of_df_utf8 <- list(df_utf8)
  lst_of_df_unknown <- list(df_unknown_1, df_unknown_2)

  expect_equal(vec_match(lst_of_df_unknown, lst_of_df_unknown), 1:2)
  expect_equal(vec_in(lst_of_df_unknown, lst_of_df_unknown), c(TRUE, TRUE))

  expect_equal(vec_match(lst_of_df_utf8, lst_of_df_unknown), 2L)
  expect_equal(vec_in(lst_of_df_utf8, lst_of_df_unknown), TRUE)
})

# splits ------------------------------------------------------------------

test_that("can split empty vector", {
  out <- vec_split(integer(), character())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, character())
  expect_equal(out$val, list_of(.ptype = integer()))
})

test_that("split data frame with data frame", {
  df <- data.frame(x = c(1, 1, 2), y = c(1, 1, 1))
  out <- vec_split(df, df)

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, data.frame(x = c(1, 2), y = c(1, 1)))
  expect_equal(out$val, list_of(
    data.frame(x = c(1, 1), y = c(1, 1)),
    data.frame(x = 2, y = 1)
  ))
})

test_that("x and by must be same size", {
  expect_error(
    vec_split(1:3, 1:2),
    "same size"
  )
})

test_that("split takes the equality proxy (#375)", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_identical(nrow(vec_split(1:3, x)), 2L)
})

# split id ---------------------------------------------------------------

test_that("can locate unique groups of an empty vector", {
  out <- vec_split_id(integer())

  expect_s3_class(out, "data.frame")
  expect_equal(out$key, integer())
  expect_equal(out$id, list_of(.ptype = integer()))
})

test_that("can locate unique groups of a data frame", {
  df <- data_frame(x = c(1, 1, 1, 2, 2), y = c("a", "a", "b", "a", "b"))
  out <- vec_split_id(df)

  expect_equal(nrow(out), 4L)
  expect_equal(out$key, vec_unique(df))
})

test_that("can locate unique groups of a data frame with a list column", {
  df <- data_frame(x = list(1:2, 1:2, "a", 5.5, "a"))
  out <- vec_split_id(df)

  expect_equal(nrow(out), 3L)
  expect_equal(out$key, vec_unique(df))
})

test_that("`x` must be a vector", {
  expect_error(vec_split_id(environment()), class = "vctrs_error_scalar_type")
})

test_that("`key` column retains full type information", {
  x <- factor(letters[c(1, 2, 1)], levels = letters[1:3])
  out <- vec_split_id(x)

  expect_equal(levels(out$key), levels(x))
})

test_that("`key` and `value` retain names", {
  x <- c(a = 1, b = 2, c = 1, a = 1)
  split <- vec_split(x, x)
  expect_identical(split$key, c(a = 1, b = 2))
  expect_identical(split$val[[1]], c(a = 1, c = 1, a = 1))
  expect_identical(split$val[[2]], c(b = 2))
})

test_that("vec_split_id takes the equality proxy", {
  scoped_comparable_tuple()
  x <- tuple(c(1, 2, 1), 1:3)
  expect_equal(vec_split_id(x)$key, x[1:2])
  expect_equal(vec_split_id(x)$id, list_of(c(1L, 3L), 2L))

  x <- as.POSIXlt(new_datetime(c(1, 2, 1)))
  expect_equal(vec_split_id(x)$key, x[1:2])
  expect_equal(vec_split_id(x)$id, list_of(c(1L, 3L), 2L))
})
