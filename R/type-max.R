vectype_max <- function(x, y, strict = TRUE) {
  UseMethod("vectype_max")
}

vectype_max.vec_type <- function(x, y, strict = TRUE) {
  y <- as_vec_type(y)
  vec_type(vectype_max(x$prototype, y$prototype, strict = strict))
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
  } else if (!strict && is_bare_character(y)) {
    character()
  } else {
    fallback(list(), x, y, strict = strict)
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

vectype_max.difftime <- function(x, y, strict = TRUE) {
  if (is_null(y)) {
    x[0]
  } else if (inherits(y, "difftime")) {
    out <- x[0]

    if (!identical(units(x), units(y))) {
      units(out) <- "secs"
    }

    out
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


# Data frames -------------------------------------------------------------

vectype_max.data.frame <- function(x, y, strict = TRUE) {
  common <- intersect(names(x), names(y))
  only_x <- setdiff(names(x), names(y))
  only_y <- setdiff(names(y), names(x))

  # Find types
  if (length(common) > 0) {
    common_types <- map2(x[common], y[common], vectype_max, strict = strict)
  } else {
    common_types <- list()
  }
  only_x_types <- map(x[only_x], vec_subset, 0L)
  only_y_types <- map(y[only_y], vec_subset, 0L)

  # Combine, restore variable order, and turn into a data frame
  out <- c(common_types, only_x_types, only_y_types)
  out <- out[c(names(x), setdiff(names(y), names(x)))]
  structure(
    out,
    class = "data.frame",
    row.names = .set_row_names(0L)
  )
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
