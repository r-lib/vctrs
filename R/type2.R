#' Find the common type for a pair of vector types
#'
#' `vec_type2()` finds the common type for a pair of vectors, or dies trying.
#' It forms the foundation of the vctrs type system, along with [vec_cast()].
#' It should generally not be called by R users, but is important for R
#' developers.
#'
#' @section Lifecycle:
#' This function is experimental, and we plan that implementation will change
#' substantially in the future. `vec_type()` does double-dispatch, but
#' currently with a combination of S3 and nested `if`-`else` blocks. In
#' the future we will provide an extensible implemntation.
#'
#' @section Coercion rules:
#' vctrs thinks of the vector types as forming a partially ordered set, or
#' poset. Then finding the common type from a set of types is a matter of
#' finding the least-upper-bound; if the least-upper-bound does not exist,
#' there is no common type. This is the case for many pairs of 1d vectors.
#'
#' The poset of the most important base vectors is shown below:
#' (where datetime stands for `POSIXt`, and date for `Date`)
#'
#' \figure{coerce.png}
#'
#' @keywords internal
#' @param x,y Either vector types produced by [vec_type()], or actual vectors.
#' @export
vec_type2 <- function(x, y) {
  UseMethod("vec_type2")
}

#' @export
vec_type2.vec_type <- function(x, y) {
  y <- as_vec_type(y)
  vec_type(vec_type2(x$prototype, y$prototype))
}

#' @export
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
  if (is_null(y) || inherits(y, "Date")) {
    x[0]
  } else if (inherits(y, "POSIXt")) {
    x <- union_tzone(x, y)
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
  } else if (inherits(y, "tbl_df")) {
    new_tibble(df_col_type2(x, y), n = 0)
  } else if (is.data.frame(y)) {
    new_data_frame(df_col_type2(x, y), n = 0)
  } else {
    abort_no_max_type(vec_type(x), vec_type(y))
  }
}

#' @export
vec_type2.tbl_df <- function(x, y) {
  if (is_null(y)) {
    x
  } else if (is.data.frame(y)) {
    new_tibble(df_col_type2(x, y), n = 0L)
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
