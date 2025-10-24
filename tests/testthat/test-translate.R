# ------------------------------------------------------------------------------
# vec_normalize_encoding()

test_that("get expected normalized-ness of various encodings", {
  x <- unlist(encodings(bytes = TRUE), use.names = FALSE)

  # For the `°C` strings:
  # - `TRUE`  for marked as `CE_UTF8`
  # - `FALSE` for marked as `CE_NATIVE`, even if `utf8locale = true`
  # - `FALSE` for marked as `CE_LATIN1`
  # - `FALSE` for marked as `CE_BYTES`
  expect_identical(
    chr_is_normalized(x),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("can translate a character vector of various encodings (#553)", {
  x <- unlist(encodings(), use.names = FALSE)

  results <- vec_normalize_encoding(x)

  expect_equal_encoding(results, encodings()$utf8)
})

test_that("translates all encodings to UTF-8", {
  encs <- encodings()

  for (enc in encs) {
    expect_equal_encoding(vec_normalize_encoding(enc), encodings()$utf8)
  }
})

test_that("can translate a list containing character vectors with different encodings", {
  results <- vec_normalize_encoding(encodings())
  results <- unlist(results)

  expect_equal_encoding(results, encodings()$utf8)
})

test_that("translation fails purposefully with any bytes", {
  expect_error(
    vec_normalize_encoding(encoding_bytes()),
    "translating strings with \"bytes\" encoding"
  )
})

test_that("translation fails purposefully when mixing with bytes with other encodings", {
  for (enc in encodings()) {
    x <- c(encoding_bytes(), enc)
    expect_error(
      vec_normalize_encoding(x),
      "translating strings with \"bytes\" encoding"
    )
  }
})

test_that("attributes are kept on translation (#599)", {
  encs <- encodings()

  x <- c(encs$utf8, encs$latin1)
  x <- structure(x, names = c("a", "b"), extra = 1)

  expect_equal(attributes(vec_normalize_encoding(x)), attributes(x))
})

test_that("translation is robust against scalar types contained in lists (#633)", {
  x <- list(a = z ~ y, b = z ~ z)
  expect_equal(vec_normalize_encoding(x), x)
})

test_that("translation can still occur even if a scalar type is in a list", {
  encs <- encodings()
  x <- list(a = z ~ y, b = encs$latin1)

  result <- vec_normalize_encoding(x)

  expect_equal_encoding(result$b, encs$utf8)
})

test_that("translation occurs inside scalars contained in a list", {
  encs <- encodings()

  scalar <- structure(list(x = encs$latin1), class = "scalar_list")
  lst <- list(scalar)

  result <- vec_normalize_encoding(lst)

  expect_equal_encoding(result[[1]]$x, encs$utf8)
})

test_that("translation treats data frames elements of lists as lists (#1233)", {
  encs <- encodings()

  field <- c(encs$utf8, encs$latin1)

  a <- new_rcrd(list(field = field))
  df <- data.frame(a = a, b = 1:2)
  x <- list(df)

  # Recursive proxy won't proxy list elements,
  # so the rcrd column in the data frame won't get proxied
  proxy <- vec_proxy_equal(x)

  result <- vec_normalize_encoding(proxy)

  expect_identical(result, x)

  result_field <- field(result[[1]]$a, "field")
  expect_field <- c(encs$utf8, encs$utf8)

  expect_equal_encoding(result_field, expect_field)
})

test_that("attributes are translated", {
  utf8 <- encodings()$utf8
  latin1 <- encodings()$latin1

  a <- structure(1, enc = utf8)
  b <- structure(1, enc = latin1)
  c <- structure(1, enc1 = utf8, enc2 = list(latin1), enc3 = latin1)
  x <- list(a, b, c)

  result <- vec_normalize_encoding(x)

  a_enc <- attr(result[[1]], "enc")
  b_enc <- attr(result[[2]], "enc")
  c_enc1 <- attr(result[[3]], "enc1")
  c_enc2 <- attr(result[[3]], "enc2")[[1]]
  c_enc3 <- attr(result[[3]], "enc3")

  expect_equal_encoding(a_enc, utf8)
  expect_equal_encoding(b_enc, utf8)
  expect_equal_encoding(c_enc1, utf8)
  expect_equal_encoding(c_enc2, utf8)
  expect_equal_encoding(c_enc3, utf8)

  expect <- list(
    structure(1, enc = utf8),
    structure(1, enc1 = utf8, enc2 = list(utf8), enc3 = utf8)
  )

  expect_identical(vec_unique(x), expect)
})

test_that("attributes are translated recursively", {
  utf8 <- encodings()$utf8
  latin1 <- encodings()$latin1

  nested <- structure(1, latin1 = latin1)
  x <- structure(2, nested = nested, foo = 1, latin1 = latin1)

  result <- vec_normalize_encoding(x)
  attrib <- attributes(result)
  attrib_nested <- attributes(attrib$nested)

  expect_equal_encoding(attrib$latin1, utf8)
  expect_equal_encoding(attrib_nested$latin1, utf8)
})

test_that("NAs aren't converted to 'NA' (#1291)", {
  utf8 <- c(NA, encodings()$utf8)
  latin1 <- c(NA, encodings()$latin1)

  result1 <- vec_normalize_encoding(utf8)
  result2 <- vec_normalize_encoding(latin1)

  expect_equal_encoding(result1, utf8)
  expect_equal_encoding(result2, utf8)

  expect_identical(result1[[1]], NA_character_)
  expect_identical(result2[[1]], NA_character_)
})
