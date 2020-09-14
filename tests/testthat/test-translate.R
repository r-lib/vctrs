# ------------------------------------------------------------------------------
# vec_normalize_encoding()

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
    expect_error(vec_normalize_encoding(x), "translating strings with \"bytes\" encoding")
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

# FIXME: Should we translate attributes to fix this? Can it be done efficiently?
test_that("attributes are currently not translated", {
  utf8 <- encodings()$utf8
  latin1 <- encodings()$latin1

  a <- structure(1, enc = utf8)
  b <- structure(1, enc = latin1)
  x <- list(a, b)

  result <- vec_normalize_encoding(x)

  a_enc <- attr(result[[1]], "enc")
  b_enc <- attr(result[[2]], "enc")

  # Ideally both would be utf8
  expect_equal_encoding(a_enc, utf8)
  expect_equal_encoding(b_enc, latin1)

  # Ideally the list elements are considered duplicates
  expect_identical(vec_unique(x), x)
})
