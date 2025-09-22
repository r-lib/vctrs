encodings <- function(bytes = FALSE) {
  string <- "\u00B0C"

  utf8 <- iconv(string, from = Encoding(string), to = "UTF-8")
  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)
  latin1 <- iconv(string, from = Encoding(string), to = "latin1")

  out <- list(utf8 = utf8, unknown = unknown, latin1 = latin1)

  if (bytes) {
    out <- list2(!!!out, bytes = encoding_bytes())
  }

  out
}

encoding_bytes <- function() {
  string <- "\u00B0C"

  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)

  bytes <- unknown
  Encoding(bytes) <- "bytes"

  bytes
}

expect_equal_encoding <- function(object, expected) {
  args <- vec_recycle_common(object, expected)
  expect_equal(Encoding(args[[1L]]), Encoding(args[[2L]]))
}
