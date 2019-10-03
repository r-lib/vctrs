
try_cat <- function(expr) {
  cat(paste0("> ", as_label(substitute(expr)), ":\n\n"))

  out <- tryCatch(expr, error = function(err) {
    cat(paste0("Error: ", err$message, "\n"))
  })

  cat("\n\n\n")
  out
}
