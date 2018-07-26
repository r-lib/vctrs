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

coerces_to <- function(x, y, ...) {
  tryCatch({
    type <- vectype_max(x, y, ...)
    vec_type(type)
  }, error = function(e) {
    NA_character_
  })
}

maxtype_mat <- function(types) {
  names(types) <- map_chr(types, vec_type)

  grid <- as_tibble(expand.grid(x = types, y = types))
  grid$max <- map2_chr(grid$x, grid$y, coerces_to)

  matrix(
    grid$max,
    nrow = length(types),
    dimnames = list(names(types), names(types))
  )
}
