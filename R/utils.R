indent <- function(x, n) {
  pad <- strrep(" ", n)
  map_chr(x, gsub, pattern = "(\n+)", replacement = paste0("\\1", pad))
}

ones <- function(...) {
  array(1, dim = c(...))
}

vec_coerce_bare <- function(x, type) {
  # Unexported wrapper around Rf_coerceVector()
  coerce <- env_get(ns_env("rlang"), "vec_coerce")
  coerce(x, type)
}


# Matches the semantics of c() - based on experimenting with the output
# of c(), not reading the source code.
outer_names <- function(names, outer, n) {
  .Call(vctrs_outer_names, names, outer, vec_cast(n, int()))
}

has_inner_names <- function(x) {
  !all(map_lgl(map(x, vec_names), is.null))
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}


set_partition <- function(x, y) {
  list(
    both = intersect(x, y),
    only_x = setdiff(x, y),
    only_y = setdiff(y, x)
  )
}

all_equal <- function(x) all(x == x[[1]])

inline_list <- function(title, x, width = getOption("width"), quote = "") {
  label_width <- width - nchar(title)
  x <- glue::glue_collapse(
    encodeString(x, quote = quote),
    sep = ", ",
    width = label_width
  )
  paste0(title, x)
}

#' Destructuring assignment
#'
#' See \code{zeallot::\link[zeallot]{\%<-\%}} for details.
#' @importFrom zeallot %<-%
#' @export
#' @rdname unpack-assign
#' @name %<-%
#' @keywords internal
`%<-%`

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

compact <- function(x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

# From rlang
friendly_type_of <- function(x, length = FALSE) {
  if (is.object(x)) {
    return(sprintf("a `%s` object", paste_classes(x)))
  }

  friendly <- as_friendly_type(typeof(x))

  if (length && is_vector(x)) {
    friendly <- paste0(friendly, sprintf(" of length %s", length(x)))
  }

  friendly
}
as_friendly_type <- function(type) {
  switch(type,
    logical = "a logical vector",
    integer = "an integer vector",
    numeric = ,
    double = "a double vector",
    complex = "a complex vector",
    character = "a character vector",
    raw = "a raw vector",
    string = "a string",
    list = "a list",

    NULL = "NULL",
    environment = "an environment",
    externalptr = "a pointer",
    weakref = "a weak reference",
    S4 = "an S4 object",

    name = ,
    symbol = "a symbol",
    language = "a call",
    pairlist = "a pairlist node",
    expression = "an expression vector",
    quosure = "a quosure",
    formula = "a formula",

    char = "an internal string",
    promise = "an internal promise",
    ... = "an internal dots object",
    any = "an internal `any` object",
    bytecode = "an internal bytecode object",

    primitive = ,
    builtin = ,
    special = "a primitive function",
    closure = "a function",

    type
  )
}
paste_classes <- function(x) {
  paste(class(x), collapse = "/")
}

paste_line <- function (...) {
  paste(chr(...), collapse = "\n")
}

has_dim <- function(x) {
  !is.null(attr(x, "dim"))
}
