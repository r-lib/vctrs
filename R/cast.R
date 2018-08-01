#' Export cast a vector to specified type
#'
#' Casting supports a wider range of transformations that are automatically
#' imputed by coercion (e.g. with [vec_coerce()]).
#'
#' @section Casting rules:
#'
#' Casting is more flexible than coercion, and allows for the possibility of
#' information loss. This diagram summarises possible coercions. `vec_cast()`
#' from any type connected to another type, provided that the arrows are
#' followed in only one direction. For example you can cast from logical to
#' character, and list to time, but you can not cast from logical to datetime.
#'
#' \figure{cast.png}
#'
#' The rules for coercing from a list a fairly strict: each component of the
#' list must be of length 1, and must be coercible to type `to`.
#'
#' @param x Vector to cast.
#' @param to Type to cast to.
#' @return A vector the same length as `x` with the same type as `to`,
#'   or an error if the cast is not possible. A warning is generated if
#'   information is lost when casting between compatible types (i.e. when
#'   there is no 1-to-1 mapping for a specific value).
#' @export
#' @keywords internal
#' @examples
#' # x is a double, but no information is lost
#' vec_cast(1, integer())
#'
#' # Information is lost so a warning is generated
#' vec_cast(1.5, integer())
#'
#' # No sensible coercion is possible so an error is generated
#' \dontrun{
#' vec_cast(1.5, factor("a"))
#' }
#'
vec_cast <- function(x, to) {
  UseMethod("vec_cast", to)
}

# Base vectors --------------------------------------------------------------

#' @export
vec_cast.NULL <- function(x, to) {
  x
}

#' @export
vec_cast.logical <- function(x, to) {
  if (is_null(x)) {
    x
  } else if (is_bare_logical(x)) {
    x
  } else if (is_bare_integer(x)) {
    warn_cast_lossy_vector(x, to, !x %in% c(0L, 1L))
    vec_coerce_bare(x, "logical")
  } else if (is_bare_double(x)) {
    warn_cast_lossy_vector(x, to, !x %in% c(0, 1))
    vec_coerce_bare(x, "logical")
  } else if (is_bare_character(x)) {
    warn_cast_lossy_vector(x, to, !toupper(x) %in% c("T", "F", "TRUE", "FALSE"))
    vec_coerce_bare(x, "logical")
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.integer <- function(x, to) {
  if (is_null(x)) {
    x
  } else if (is_bare_logical(x)) {
    vec_coerce_bare(x, "integer")
  } else if (is_bare_integer(x)) {
    x
  } else if (is_bare_double(x) || is_bare_character(x)) {
    out <- suppressWarnings(vec_coerce_bare(x, "integer"))
    warn_cast_lossy_vector(x, to, (out != x) | xor(is.na(x), is.na(out)))
    out
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.double <- function(x, to) {
  if (is_null(x)) {
    x
  } else if (is_bare_logical(x) || is_bare_integer(x)) {
    vec_coerce_bare(x, "double")
  } else if (is_bare_double(x)) {
    x
  } else if (is_bare_character(x)) {
    out <- suppressWarnings(vec_coerce_bare(x, "double"))
    warn_cast_lossy_vector(x, to, (out != x) | xor(is.na(x), is.na(out)))
    out
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.character <- function(x, to) {
  if (is_null(x)) {
    x
  } else if (is_bare_atomic(x)) {
    vec_coerce_bare(x, "character")
  } else if (inherits(x, "difftime")) {
    paste(x, units(x))
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    as.character(x)
  }
}

#' @export
vec_cast.list <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is_repeated(x)) {
    warn_cast_lossy(from = x, to = to)
    as.list(x)
  } else {
    as.list(x)
  }
}

#' @export
vec_cast.repeated <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is_repeated(x) || is_bare_list(x)) {
    as_repeated(x, .type = attr(to, "type"))
  } else {
    abort_no_cast(x, to)
  }
}

# S3 vectors --------------------------------------------------------------

#' @export
vec_cast.factor <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is.character(x) || is.factor(x))  {
    if (length(levels(to)) == 0L) {
      factor(as.character(x), levels = unique(x), ordered = is.ordered(to))
    } else {
      warn_cast_lossy_vector(x, to, !x %in% levels(to))
      factor(x, levels = levels(to), ordered = is.ordered(to))
    }
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.Date <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is_bare_double(x)) {
    as.Date(x, origin = "1970-01-01")
  } else if (is_bare_character(x)) {
    as.Date(x, format = "%Y-%m-%d")
  } else if (inherits(x, "Date")) {
    x
  } else if (inherits(x, "POSIXt")) {
    out <- as.Date(x)
    warn_cast_lossy_vector(x, to, abs(x - as.POSIXct(out)) > 1e-9)
    out
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.POSIXt <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is_bare_double(x)) {
    x <- as.POSIXct(x, origin = "1970-01-01")
    attr(x, "tzone") <- attr(to, "tzone")
    x
  } else if (is_bare_character(x)) {
    as.POSIXct(x, tz = "UTC")
  } else if (inherits(x, "Date")) {
    x <- as.POSIXct(x)
    attr(x, "tzone") <- attr(to, "tzone")
    x
  } else if (inherits(x, "POSIXt")) {
    attr(x, "tzone") <- attr(to, "tzone")
    x
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.difftime <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is_bare_double(x)) {
    structure(
      as.double(x), # strip attributes
      class = "difftime",
      units = units(to)
    )
  } else if (inherits(x, "difftime")) {
    if (identical(units(x), units(to))) {
      x
    } else {
      # Hack: I can't see any obvious way of changing the units
      origin <- as.POSIXct(0, origin = "1970-01-01")
      difftime(origin, origin - x, units = units(to))
    }
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.data.frame <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is.data.frame(x)) {
    # Coerce common columns
    common <- intersect(names(x), names(to))
    x[common] <- map2(x[common], to[common], vec_cast)

    # Add new columns
    from_type <- setdiff(names(to), names(x))
    x[from_type] <- map(to[from_type], vec_na, n = vec_length(x))

    # Warn about dropped columns
    dropped <- setdiff(names(x), names(to))
    if (length(dropped) > 0 ) {
      warn_cast_lossy_dataframe(x, to, dropped)
    }

    x[c(common, from_type)]
  } else {
    abort_no_cast(x, to)
  }
}

# Helpers -----------------------------------------------------------------

cast_from_list <- function(x, to) {
  ns <- map_int(x, length)
  if (any(ns != 1)) {
    abort_no_cast(x, to, "All list elements are not length 1")
  }

  n <- length(x)
  out <- vec_na(to, n)

  for (i in seq_len(n)) {
    out[[i]] <- vec_cast(x[[i]], to)
  }

  out
}

abort_no_cast <- function(from, to, details = NULL) {
  from <- as_vec_type(from)
  to <- as_vec_type(to)

  msg <- glue::glue("Can't cast {from} to {to}")
  if (!is.null(details)) {
    msg <- paste0(msg, "\n", details)
  }
  abort(
    "error_no_cast",
    message = msg,
    from = from,
    to = to,
    details = details
  )
}

warn_cast_lossy <- function(message = NULL, .subclass = NULL, from, to, ..., class) {
  from <- as_vec_type(from)
  to <- as_vec_type(to)

  if (is.null(message)) {
    message <- glue::glue("Lossy conversion from {from} to {to}")
  }

  warn(
    c(.subclass, "warning_cast_lossy"),
    message = message,
    from = from,
    to = to,
    ...
  )
}

warn_cast_lossy_vector <- function(from, to, is_lossy) {
  which <- which(is_lossy)
  if (length(which) == 0) {
    return()
  }

  from <- as_vec_type(from)
  to <- as_vec_type(to)

  pos <- glue::glue_collapse(which, width = 80)
  msg <- glue::glue("
    Lossy conversion from {from} to {to}
    At positions: {pos}"
  )

  warn_cast_lossy(
    "warning_cast_lossy_vector",
    message = msg,
    from = from,
    to = to,
    which = which
  )
}

warn_cast_lossy_dataframe <- function(from, to, dropped) {
  from <- as_vec_type(from)
  to <- as_vec_type(to)

  vars <- glue::glue_collapse(dropped, width = 80)
  msg <- glue::glue("
    Lossy conversion from data.frame to data.frame
    Dropped variables: {vars}"
  )

  warn_cast_lossy(
    "warning_cast_lossy_dataframe",
    message = msg,
    from = from,
    to = to,
    dropped = dropped
  )
}
