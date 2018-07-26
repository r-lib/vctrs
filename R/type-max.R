vectype_max <- function(x, y, strict = TRUE) {
  UseMethod("vectype_max")
}

vectype_max.NULL <- function(x, y, strict = TRUE) {
  vec_subset(y, 0L)
}

vectype_max.logical <- function(x, y, strict = TRUE) {
  if (is_null(y)) {
    logical()
  } else if (is_bare_logical(y)) {
    logical()
  } else if (is_bare_integer(y)) {
    integer()
  } else if (is_bare_double(y)) {
    double()
  } else {
    list_fallback(x, y, strict = strict)
  }
}

vectype_max.integer <- function(x, y, strict = TRUE) {
  if (is_null(y)) {
    integer()
  } else if (is_bare_logical(y)) {
    integer()
  } else if (is_bare_integer(y)) {
    integer()
  } else if (is_bare_double(y)) {
    double()
  } else {
    list_fallback(x, y, strict = strict)
  }
}

vectype_max.double <- function(x, y, strict = TRUE) {
  if (is_null(y)) {
    double()
  } else if (is_bare_logical(y)) {
    double()
  } else if (is_bare_integer(y)) {
    double()
  } else if (is_bare_double(y)) {
    double()
  } else {
    list_fallback(x, y, strict = strict)
  }
}

vectype_max.character <- function(x, y, strict = TRUE) {
  if (is_null(y) || is_bare_character(y)) {
    character()
  } else {
    list_fallback(x, y, strict = strict)
  }
}

vectype_max.list <- function(x, y, strict = TRUE) {
  if (is_null(y) || is_bare_list(y)) {
    list()
  } else {
    list_fallback(x, y, strict = strict)
  }
}

# Helpers -----------------------------------------------------------------

list_fallback <- function(x, y, strict = TRUE) {
  if (!strict) {
    list()
  } else {
    # should be classed condition
    stop("No maximum type for ", vec_type(x), " and ", vec_type(y), call. = FALSE)
  }
}
