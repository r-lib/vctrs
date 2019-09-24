on_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}
