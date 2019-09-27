encodings <- function() {
  string <- "\u00B0C"

  utf <- iconv(string, from = Encoding(string), to = "UTF-8")
  unknown <- iconv(string, from = Encoding(string), to = "", mark = FALSE)
  latin1 <- iconv(string, from = Encoding(string), to = "latin1")

  bytes <- unknown
  Encoding(bytes) <- "bytes"

  list(utf = utf, unknown = unknown, latin1 = latin1, bytes = bytes)
}
