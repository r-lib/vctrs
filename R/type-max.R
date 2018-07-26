vectype_max <- function(x, y, strict = TRUE) {
  UseMethod("vectype_max")
}

vectype_max.NULL <- function(x, y, strict = TRUE) {
  vec_subset(y, 0L)
}

# Numeric-ish ----------------------------------------------------------

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
    fallback(list(), x, y, strict = strict)
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
    fallback(list(), x, y, strict = strict)
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
    fallback(list(), x, y, strict = strict)
  }
}


# Characters and factors --------------------------------------------------

vectype_max.character <- function(x, y, strict = TRUE) {
  if (is_null(y) || is_bare_character(y)) {
    character()
  } else if (!strict && is.factor(y)) {
    character()
  } else {
    fallback(list(), x, y, strict = strict)
  }
}

vectype_max.factor <- function(x, y, strict = TRUE) {
  if (is_null(y)) {
    vec_subset(x, 0L)
  } else if (is.factor(y)) {
    switch(
      set_compare(levels(x), levels(y)),
      equal = vec_subset(x, 0L),
      x_in_y = vec_subset(y, 0L),
      y_in_x = vec_subset(x, 0L),
      fallback(character(), x, y, strict = strict) # needs custom error
    )
  } else if (!strict && is.character(y)) {
    character()
  } else {
    fallback(list(), x, y, strict = TRUE)
  }
}

# Date/times --------------------------------------------------------------

vectype_max.Date <- function(x, y, strict = TRUE) {
  if (is_null(y) || inherits(y, "Date")) {
    x[0]
  } else if (inherits(y, "POSIXt")) {
    y[0]
  } else {
    fallback(list(), x, y, strict = strict)
  }
}

vectype_max.POSIXt <- function(x, y, strict = TRUE) {
  if (is_null(y) || inherits(y, "Date") || inherits(y, "POSIXt")) {
    x[0]
  } else {
    fallback(list(), x, y, strict = strict)
  }
}

# Lists -------------------------------------------------------------------

vectype_max.list <- function(x, y, strict = TRUE) {
  if (is_null(y) || is_bare_list(y)) {
    list()
  } else {
    fallback(x, y, list(), strict = strict)
  }
}

# Helpers -----------------------------------------------------------------

fallback <- function(fallback, x, y, strict = TRUE) {
  if (!strict) {
    fallback
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

maxtype_mat <- function(types, strict = TRUE) {
  names(types) <- map_chr(types, vec_type)

  grid <- expand.grid(x = types, y = types)
  grid$max <- map2_chr(grid$x, grid$y, coerces_to, strict = strict)

  matrix(
    grid$max,
    nrow = length(types),
    dimnames = list(names(types), names(types))
  )
}
