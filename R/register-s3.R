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
#' As of R 3.6.0, a similar effect can be accomplished by using "delayed method
#' registration", by placing the following in your `NAMESPACE` file:
#'
#' ```
#' if (getRversion() >= "3.6.0") {
#'   S3method(package::generic, class)
#' }
#' ```
#'
#' @param generic Name of the generic in the form `pkg::generic`.
#' @param class Name of the class
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
#'   vec_method_register("pillar::pillar_shaft", "vctrs_vctr")
#'   vec_method_register("tibble::type_sum", "vctrs_vctr")
#' }
#' @keywords internal
vec_method_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

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
