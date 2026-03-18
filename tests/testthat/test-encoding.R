test_that("get expected re-encoded-ness of various encodings", {
  x <- unlist(encodings(bytes = TRUE), use.names = FALSE)

  # For the `°C` strings:
  # - `TRUE`  for marked as `CE_UTF8`
  # - `FALSE` for marked as `CE_NATIVE`, even if `utf8locale = true`
  # - `FALSE` for marked as `CE_LATIN1`
  # - `FALSE` for marked as `CE_BYTES`
  expect_identical(
    chr_is_ascii_or_utf8(x),
    c(TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("can UTF-8 encode a character vector of various encodings (#553)", {
  x <- unlist(encodings(), use.names = FALSE)

  results <- obj_encode_utf8(x)

  expect_equal_encoding(results, encodings()$utf8)
})

test_that("UTF-8 encodes all encodings", {
  encs <- encodings()

  for (enc in encs) {
    expect_equal_encoding(obj_encode_utf8(enc), encodings()$utf8)
  }
})

test_that("can UTF-8 encode a list containing character vectors with different encodings", {
  results <- obj_encode_utf8(encodings())
  results <- unlist(results)

  expect_equal_encoding(results, encodings()$utf8)
})

test_that("UTF-8 encoding fails purposefully with any bytes", {
  expect_error(
    obj_encode_utf8(encoding_bytes()),
    "translating strings with \"bytes\" encoding"
  )
})

test_that("UTF-8 encoding fails purposefully when mixing with bytes with other encodings", {
  for (enc in encodings()) {
    x <- c(encoding_bytes(), enc)
    expect_error(
      obj_encode_utf8(x),
      "translating strings with \"bytes\" encoding"
    )
  }
})

test_that("attributes are kept through UTF-8 encoding (#599)", {
  encs <- encodings()

  x <- c(encs$utf8, encs$latin1)
  x <- structure(x, names = c("a", "b"), extra = 1)

  expect_equal(attributes(obj_encode_utf8(x)), attributes(x))
})

test_that("UTF-8 encoding is robust against scalar types contained in lists (#633)", {
  x <- list(a = z ~ y, b = z ~ z)
  expect_equal(obj_encode_utf8(x), x)
})

test_that("UTF-8 encoding can still occur even if a scalar type is in a list", {
  encs <- encodings()
  x <- list(a = z ~ y, b = encs$latin1)

  result <- obj_encode_utf8(x)

  expect_equal_encoding(result$b, encs$utf8)
})

test_that("UTF-8 encoding occurs inside scalars contained in a list", {
  encs <- encodings()

  scalar <- structure(list(x = encs$latin1), class = "scalar_list")
  lst <- list(scalar)

  result <- obj_encode_utf8(lst)

  expect_equal_encoding(result[[1]]$x, encs$utf8)
})

test_that("UTF-8 encoding treats data frames elements of lists as lists (#1233)", {
  encs <- encodings()

  field <- c(encs$utf8, encs$latin1)

  a <- new_rcrd(list(field = field))
  df <- data.frame(a = a, b = 1:2)
  x <- list(df)

  # Recursive proxy won't proxy list elements,
  # so the rcrd column in the data frame won't get proxied
  proxy <- vec_proxy_equal(x)

  result <- obj_encode_utf8(proxy)

  expect_identical(result, x)

  result_field <- field(result[[1]]$a, "field")
  expect_field <- c(encs$utf8, encs$utf8)

  expect_equal_encoding(result_field, expect_field)
})

test_that("attributes are UTF-8 encoded", {
  utf8 <- encodings()$utf8
  latin1 <- encodings()$latin1

  a <- structure(1, enc = utf8)
  b <- structure(1, enc = latin1)
  c <- structure(1, enc1 = utf8, enc2 = list(latin1), enc3 = latin1)
  x <- list(a, b, c)

  result <- obj_encode_utf8(x)

  a_enc <- attr(result[[1]], "enc")
  b_enc <- attr(result[[2]], "enc")
  c_enc1 <- attr(result[[3]], "enc1")
  c_enc2 <- attr(result[[3]], "enc2")[[1]]
  c_enc3 <- attr(result[[3]], "enc3")

  expect_utf8_encoded(a_enc)
  expect_utf8_encoded(b_enc)
  expect_utf8_encoded(c_enc1)
  expect_utf8_encoded(c_enc2)
  expect_utf8_encoded(c_enc3)

  expect <- list(
    structure(1, enc = utf8),
    structure(1, enc1 = utf8, enc2 = list(utf8), enc3 = utf8)
  )

  expect_identical(vec_unique(x), expect)

  # Ensure nothing has changed in the original objects
  a_enc <- attr(a, "enc")
  b_enc <- attr(b, "enc")
  c_enc1 <- attr(c, "enc1")
  c_enc2 <- attr(c, "enc2")[[1]]
  c_enc3 <- attr(c, "enc3")

  expect_utf8_encoded(a_enc)
  expect_latin1_encoded(b_enc)
  expect_utf8_encoded(c_enc1)
  expect_latin1_encoded(c_enc2)
  expect_latin1_encoded(c_enc3)
})

test_that("attributes are UTF-8 encoded recursively", {
  utf8 <- encodings()$utf8
  latin1 <- encodings()$latin1

  nested <- structure(1, latin1 = latin1)
  x <- structure(2, nested = nested, foo = 1, latin1 = latin1)

  result <- obj_encode_utf8(x)
  attrib <- attributes(result)
  attrib_nested <- attributes(attrib$nested)

  expect_equal_encoding(attrib$latin1, utf8)
  expect_equal_encoding(attrib_nested$latin1, utf8)
})

test_that("NAs aren't converted to 'NA' (#1291)", {
  utf8 <- c(NA, encodings()$utf8)
  latin1 <- c(NA, encodings()$latin1)

  result1 <- obj_encode_utf8(utf8)
  result2 <- obj_encode_utf8(latin1)

  expect_equal_encoding(result1, utf8)
  expect_equal_encoding(result2, utf8)

  expect_identical(result1[[1]], NA_character_)
  expect_identical(result2[[1]], NA_character_)
})
