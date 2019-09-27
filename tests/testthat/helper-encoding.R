encodings <- function() {
  string <- "\u00B0C"

  utf <- iconv(string, from = Encoding(string), to = "UTF-8")
  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)
  latin1 <- iconv(string, from = Encoding(string), to = "latin1")

  list(utf = utf, unknown = unknown, latin1 = latin1)
}

encoding_bytes <- function() {
  string <- "\u00B0C"

  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)

  bytes <- unknown
  Encoding(bytes) <- "bytes"

  bytes
}
