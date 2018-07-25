hash <- function(x) {
  substr(digest::digest(x), 1, 5)
}
