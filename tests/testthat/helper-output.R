
try_cat <- function(expr) {
  cat(paste0("> ", as_label(substitute(expr)), ":\n\n"))

  out <- tryCatch(expr, error = function(err) {
    cat(paste0("Error: ", err$message, "\n"))
  })

  cat("\n\n\n")
  out
}

skip_unless_utf8 <- function() {
  skip_if(!cli_is_utf8_output())
}

# Until we add a utf8 parameter to `verify_output()`
verify_output <- function(path, code, width = 80, crayon = FALSE) {
  skip_unless_utf8()
  testthat::verify_output(
    path = path,
    code = !!enquo(code),
    width = width,
    crayon = crayon
  )
}
