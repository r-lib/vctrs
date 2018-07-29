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
    warn_cast_lossy(x, to, !x %in% c(0L, 1L))
    set_names(as.logical(x), names(x))
  } else if (is_bare_double(x)) {
    warn_cast_lossy(x, to, !x %in% c(0, 1))
    set_names(as.logical(x), names(x))
  } else if (is_bare_character(x)) {
    warn_cast_lossy(x, to, !toupper(x) %in% c("T", "F", "TRUE", "FALSE"))
    set_names(as.logical(x), names(x))
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
    set_names(as.integer(x), names(x))
  } else if (is_bare_integer(x)) {
    x
  } else if (is_bare_double(x) || is_bare_character(x)) {
    out <- set_names(suppressWarnings(as.integer(x)), names(x))
    warn_cast_lossy(x, to, (out != x) | xor(is.na(x), is.na(out)))
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
    set_names(as.integer(x), names(x))
  } else if (is_bare_double(x)) {
    x
  } else if (is_bare_character(x)) {
    out <- set_names(suppressWarnings(as.double(x)), names(x))
    warn_cast_lossy(x, to, (out != x) | xor(is.na(x), is.na(out)))
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
  } else {
    as.list(x)
  }
}

# S3 vectors --------------------------------------------------------------

#' @export
vec_cast.factor <- function(x, to) {
  if (is_null(x)) {
    NULL
  } else if (is.character(x))  {
    if (length(levels(to)) == 0L) {
      factor(x, levels = unique(x))
    } else {
      warn_cast_lossy(x, to, !x %in% levels(to))
      factor(x, levels = levels(to))
    }
  } else if (is.factor(x)) {
    if (identical(levels(x), levels(to)) || length(levels(to)) == 0) {
      # fast path
      x
    } else {
      warn_cast_lossy(x, to, !x %in% levels(to))
      factor(as.character(x), levels = levels(to))
    }
  } else if (is.list(x)) {
    cast_from_list(x, to)
  } else {
    abort_no_cast(x, to)
  }
}

#' @export
vec_cast.Date <- function(x, to) {
  as.Date(x)
}

#' @export
vec_cast.POSIXt <- function(x, to) {
  as.POSIXct(x)
}

#' @export
vec_cast.difftime <- function(x, to) {
  structure(
    as.double(x),
    class = "difftime",
    units = units(to)
  )
}

#' @export
vec_cast.data.frame <- function(x, to) {
  # Coerce common columns
  common <- intersect(names(x), names(to))
  x[common] <- map2(x[common], to[common], vec_cast)

  # Add new columns
  only_type <- setdiff(names(to), names(x))
  x[only_type] <- map(to[only_type], vec_na, n = vec_length(x))

  x[c(common, only_type)]
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

warn_cast_lossy <- function(from, to, is_lossy) {
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

  warn(
    "warning_cast_lossy",
    message = msg,
    from = from,
    to = to,
    which = which
  )
}
