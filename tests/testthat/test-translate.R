context("test-translate")

# ------------------------------------------------------------------------------
# obj_maybe_translate_encoding()

test_that("can translate a character vector of various encodings (#553)", {
  x <- unlist(encodings(), use.names = FALSE)

  results <- obj_maybe_translate_encoding(x)

  expect_equal_encoding(results, encodings()$utf8)
})

test_that("does not perform translation when encodings are all the same", {
  encs <- c(encodings(), list(bytes = encoding_bytes()))

  for (enc in encs) {
    x <- c(enc, enc)
    expect_equal_encoding(obj_maybe_translate_encoding(x), x)
  }
})

test_that("can translate a list containing character vectors with different encodings", {
  results <- obj_maybe_translate_encoding(encodings())
  results <- unlist(results)

  expect_equal_encoding(results, encodings()$utf8)
})

test_that("translation fails purposefully when mixing with bytes with other encodings", {
  for (enc in encodings()) {
    x <- c(encoding_bytes(), enc)
    expect_error(obj_maybe_translate_encoding(x), "translating strings with \"bytes\" encoding")
  }
})

test_that("attributes are kept on translation (#599)", {
  encs <- encodings()

  x <- c(encs$utf8, encs$latin1)
  x <- structure(x, names = c("a", "b"), extra = 1)

  expect_equal(attributes(obj_maybe_translate_encoding(x)), attributes(x))
})

test_that("translation is robust against scalar types contained in lists (#633)", {
  x <- list(a = z ~ y, b = z ~ z)
  expect_equal(obj_maybe_translate_encoding(x), x)
})

test_that("translation can still occur even if a scalar type is in a list", {
  encs <- encodings()
  x <- list(a = z ~ y, b = encs$utf8, c = encs$latin1)
  expect <- list(a = z ~ y, b = encs$utf8, c = encs$utf8)
  expect_equal(obj_maybe_translate_encoding(x), expect)
})

# ------------------------------------------------------------------------------
# obj_maybe_translate_encoding2()

test_that("can find a common encoding between various encoding combinations", {
  encs <- encodings()

  for (x_enc in encs) {
    for (y_enc in encs) {
      together <- obj_maybe_translate_encoding(c(x_enc, y_enc))

      separate <- obj_maybe_translate_encoding2(x_enc, y_enc)
      separate <- unlist(separate)

      expect_equal_encoding(separate, together)
    }
  }
})

test_that("can ignore strings containing only bytes", {
  bytes <- encoding_bytes()

  result <- obj_maybe_translate_encoding2(bytes, bytes)
  result <- unlist(result)

  expect_equal_encoding(result, bytes)
})

test_that("cannot find a common encoding when mixing bytes with other encodings", {
  encs <- encodings()
  bytes <- encoding_bytes()

  for (enc in encs) {
    expect_error(obj_maybe_translate_encoding2(enc, bytes), "translating strings with \"bytes\" encoding")
  }
})

test_that("can find a common encoding when one string has multiple encodings", {
  encs <- encodings()

  utf8_unknown <- c(encs$utf8, encs$unknown)

  together <- obj_maybe_translate_encoding(c(utf8_unknown, encs$latin1))

  separate <- obj_maybe_translate_encoding2(utf8_unknown, encs$latin1)
  separate <- unlist(separate)

  expect_equal_encoding(separate, together)
})

test_that("can find a common encoding between lists of characters with different encodings", {
  encs <- encodings()


  lst_utf8 <- list(encs$utf8)
  lst_ascii_latin1 <- list("ascii", encs$latin1)

  together <- obj_maybe_translate_encoding(c(lst_utf8, lst_ascii_latin1))
  together <- unlist(together)

  separate <- obj_maybe_translate_encoding2(lst_utf8, lst_ascii_latin1)
  separate <- unlist(separate)

  expect_equal_encoding(separate, together)


  lst_of_lst_utf8 <- list(lst_utf8)
  lst_of_lst_ascii_latin1 <- list(list("ascii"), list(encs$latin1))

  together <- obj_maybe_translate_encoding(c(lst_of_lst_utf8, lst_of_lst_ascii_latin1))
  together <- unlist(together)

  separate <- obj_maybe_translate_encoding2(lst_of_lst_utf8, lst_of_lst_ascii_latin1)
  separate <- unlist(separate)

  expect_equal_encoding(separate, together)
})

test_that("can find a common encoding with data frames with character columns", {
  encs <- encodings()

  df_utf8 <- data_frame(x = encs$utf8)
  df_unknown <- data_frame(x = encs$unknown)

  c(result1, result2) %<-% obj_maybe_translate_encoding2(df_utf8, df_unknown)

  expect_equal_encoding(result1$x, df_utf8$x)
  expect_equal_encoding(result2$x, df_utf8$x)
})

test_that("can find a common encoding with data frame subclasses with character columns", {
  encs <- encodings()

  df_utf8 <- new_data_frame(list(x = encs$utf8), class = "subclass")
  df_unknown <- new_data_frame(list(x = encs$unknown), class = "subclass")

  c(result1, result2) %<-% obj_maybe_translate_encoding2(df_utf8, df_unknown)

  expect_equal_encoding(result1$x, df_utf8$x)
  expect_equal_encoding(result2$x, df_utf8$x)
})

test_that("only columns requiring translation are affected", {
  encs <- encodings()

  df_utf8_latin1 <- data_frame(x = encs$utf8, y = encs$latin1)
  df_unknown_latin1 <- data_frame(x = encs$unknown, y = encs$latin1)

  c(result1, result2) %<-% obj_maybe_translate_encoding2(df_utf8_latin1, df_unknown_latin1)

  expect_equal_encoding(result1$y, df_utf8_latin1$y)
  expect_equal_encoding(result2$y, df_unknown_latin1$y)
})

test_that("can find a common encoding with lists of data frames with string columns", {
  encs <- encodings()

  df_utf8 <- data_frame(x = encs$utf8)
  df_unknown_1 <- data_frame(x = encs$unknown)
  df_unknown_2 <- data_frame(x = encs$unknown)

  lst_of_df_utf8 <- list(df_utf8)
  lst_of_df_unknown <- list(df_unknown_1, df_unknown_2)

  c(result1, result2) %<-% obj_maybe_translate_encoding2(lst_of_df_utf8, lst_of_df_unknown)

  expect_equal_encoding(result1[[1]]$x, df_utf8$x)
  expect_equal_encoding(result2[[1]]$x, df_utf8$x)
  expect_equal_encoding(result2[[2]]$x, df_utf8$x)
})

test_that("all elements are affected when any translation is required in a list", {
  encs <- encodings()

  lst_of_utf8 <- list(encs$utf8)

  # Both elements of the list are recursively translated
  df_latin1 <- data_frame(x = encs$latin1)
  lst_of_unknown_df_latin1 <- list(encs$unknown, df_latin1)

  c(result1, result2) %<-% obj_maybe_translate_encoding2(lst_of_utf8, lst_of_unknown_df_latin1)

  expect_equal_encoding(result1[[1]], encs$utf8)
  expect_equal_encoding(result2[[1]], encs$utf8)
  expect_equal_encoding(result2[[2]]$x, encs$utf8)
})

test_that("translation is robust against scalar types contained in lists (#633)", {
  x <- list(a = z ~ y, b = z ~ z)
  y <- list(a = c ~ d, b = e ~ f)
  expect_equal(obj_maybe_translate_encoding2(x, y), list(x, y))
})
