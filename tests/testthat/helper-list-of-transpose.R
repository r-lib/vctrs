# Infers both type and size
list_of2 <- function(...) {
  list_of(..., .ptype = NULL, .size = NULL)
}
