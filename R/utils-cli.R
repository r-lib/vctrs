parens <- function(x, left = TRUE) {
  x_lines <- strsplit(x, "\n")
  x_lines <- map(x_lines, paren, left = left)
  map_chr(x_lines, paste0, collapse = "\n")
}

paren <- function(x, left = TRUE) {
  if (length(x) <= 1) {
    if (left) {
      paste0("( ", x)
    } else {
      paste0(x, " )")
    }
  } else {
    if (left) {
      paste0(c("\u250c ", rep("\u2502 ", length(x) - 2), "\u2514 "), x)
    } else {
      paste0(format(x), c(" \u2510", rep(" \u2502", length(x) - 2), " \u2518"))
    }
  }
}

pad_height <- function(x) {
  pad <- function(x, n) c(x, rep("", n - length(x)))

  lines <- strsplit(x, "\n")
  height <- max(map_int(lines, length))
  lines <- map(lines, pad, height)
  map_chr(lines, paste0, "\n", collapse = "")
}

pad_width <- function(x) {
  lines <- strsplit(x, "\n", fixed = TRUE)

  # fix up strsplit bug
  n <- map_int(lines, length)
  lines[n == 0] <- ""

  width <- max(unlist(map(lines, nchar)))
  lines <- map(lines, format, width = width)
  map_chr(lines, paste, collapse = "\n")
}

str_backtick <- function(x) {
  paste0("`", x, "`")
}
str_is_multiline <- function(x) {
  grepl("\n", x)
}
