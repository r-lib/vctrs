encodings <- function() {
  string <- "\u00B0C"

  utf8 <- iconv(string, from = Encoding(string), to = "UTF-8")
  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)
  latin1 <- iconv(string, from = Encoding(string), to = "latin1")

  list(utf8 = utf8, unknown = unknown, latin1 = latin1)
}

encoding_bytes <- function() {
  string <- "\u00B0C"

  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)

  bytes <- unknown
  Encoding(bytes) <- "bytes"

  bytes
}

expect_equal_encoding <- function(object, expected) {
  c(object, expected) %<-% vec_recycle_common(object, expected)
  expect_equal(Encoding(object), Encoding(expected))
}
