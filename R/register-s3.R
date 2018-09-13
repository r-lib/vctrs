#' Register a method for a suggested dependency
#'
#' Generally, the recommend way to register an S3 method is to use the
#' `S3Method()` namespace directive (often generated automatically be the
#' `@export` roxygen2 tag). However, this technique requires that the generic
#' be in an imported package, and sometimes you want to suggest a package,
#' and only provide a method when that package is loaded. `vec_method_register()`
#' should be called from your package's `.onLoad()` to dynamically register
#' a method only if the generic's package is loaded.
#'
#' `vec_method_register()` is also useful when demonstrating class creation
#' in a vignette, since as of R 3.5.0, method lookup no longer always involves
#' the lexical scope.
#'
#' @param method_name Name of the method in the form `pkg::generic.class`.
#' @param method Optionally, the implementation of the method. By default,
#'   this will be found by looking for a function called `generic.class`
#'   in the package environment.
#' @export
#' @examples
#' # A typical use case is to dynamically register tibble/pillar methods
#' # for your class. That way you avoid creating a hard depedency on packages
#' # that are not essential, while still providing finer control over
#' # printing when they are used.
#'
#' .onLoad <- function(...) {
#'   vec_method_register("pillar::pillar_shaft.vctrs_vctr")
#'   vec_method_register("tibble::type_sum.vctrs_vctr")
#' }
#' @keywords internal
vec_method_register <- function(method_name, method = NULL) {
  stopifnot(is.character(method_name), length(method_name) == 1)

  pieces <- strsplit(method_name, "::|[.]")[[1]]
  stopifnot(length(pieces) == 3)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  class <- pieces[[3]]

  if (is.null(method)) {
    method <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(method))

  if (package %in% loadedNamespaces()) {
    registerS3method(generic, class, method, envir = asNamespace(package))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      registerS3method(generic, class, method, envir = asNamespace(package))
    }
  )
}
