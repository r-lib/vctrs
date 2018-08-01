vec_type2 <- function(x, y) {
  UseMethod("vec_type2")
}

vec_type2.vec_type <- function(x, y) {
  y <- as_vec_type(y)
  vec_type(vec_type2(x$prototype, y$prototype))
}

vec_type2.NULL <- function(x, y) {
  vec_subset(y, 0L)
}

# Numeric-ish ----------------------------------------------------------

#' @export
vec_type2.logical <- function(x, y) {
  if (is_null(y)) {
    logical()
  } else if (is_bare_logical(y)) {
    logical()
  } else if (is_bare_integer(y)) {
    integer()
  } else if (is_bare_double(y)) {
    double()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.integer <- function(x, y) {
  if (is_null(y)) {
    integer()
  } else if (is_bare_logical(y)) {
    integer()
  } else if (is_bare_integer(y)) {
    integer()
  } else if (is_bare_double(y)) {
    double()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.double <- function(x, y) {
  if (is_null(y)) {
    double()
  } else if (is_bare_logical(y)) {
    double()
  } else if (is_bare_integer(y)) {
    double()
  } else if (is_bare_double(y)) {
    double()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}


# Characters and factors --------------------------------------------------

#' @export
vec_type2.character <- function(x, y) {
  if (is_null(y) || is_bare_character(y) || is.factor(y)) {
    character()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.factor <- function(x, y) {
  if (is_null(y)) {
    vec_subset(x, 0L)
  } else if (is.factor(y)) {
    factor(levels = union(levels(x), levels(y)))
  } else if (is_bare_character(y)) {
    character()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.ordered <- function(x, y) {
  if (is_null(y)) {
    vec_subset(x, 0L)
  } else if (is.ordered(y)) {
    ordered(character(), levels = union(levels(x), levels(y)))
  } else if (is.factor(y)) {
    factor(levels = union(levels(x), levels(y)))
  } else if (is_bare_character(y)) {
    character()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

# Date/times --------------------------------------------------------------

#' @export
vec_type2.Date <- function(x, y) {
  if (is_null(y) || inherits(y, "Date")) {
    x[0]
  } else if (inherits(y, "POSIXt")) {
    y[0]
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.POSIXt <- function(x, y) {
  if (is_null(y) || inherits(y, "Date") || inherits(y, "POSIXt")) {
    x[0]
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.difftime <- function(x, y) {
  if (is_null(y)) {
    x[0]
  } else if (inherits(y, "difftime")) {
    out <- x[0]

    if (!identical(units(x), units(y))) {
      units(out) <- "secs"
    }

    out
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }

}

# Lists -------------------------------------------------------------------

#' @export
vec_type2.list <- function(x, y) {
  if (is_null(y) || is_bare_list(y)) {
    list()
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.list_of <- function(x, y) {
  if (is_null(y)) {
    x
  } else if (is_list_of(y)) {
    type <- vec_type2(attr(x, "type"), attr(y, "type"))
    new_list_of(list(), type)
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

# Data frames -------------------------------------------------------------

#' @export
vec_type2.data.frame <- function(x, y) {
  if (is_null(y)) {
    x
  } else if (is.data.frame(y)) {
    common <- intersect(names(x), names(y))
    only_x <- setdiff(names(x), names(y))
    only_y <- setdiff(names(y), names(x))

    # Find types
    if (length(common) > 0) {
      common_types <- map2(x[common], y[common], vec_type2)
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
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }

}

# Helpers -----------------------------------------------------------------


abort_no_max_type <- function(type_x, type_y) {
  msg <- glue::glue("No common type for {type_x} and {type_y}")
  abort(
    "error_no_max_type",
    message = msg,
    type_x = type_x,
    type_y = type_y,
  )
}
