hash <- function(x) {
  substr(digest::digest(x), 1, 5)
}

indent <- function(x, n) {
  pad <- strrep(" ", n)

  out <- Map(gsub, "\n", paste0("\n", pad), x)
  unlist(out, use.names = FALSE)
}
