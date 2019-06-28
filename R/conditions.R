#' Custom conditions for vctrs package
#'
#' These functions are called for their side effect of raising
#' errors and warnings.
#' These conditions have custom classes and structures to make
#' testing easier.
#'
#' @param x,y Vectors
#' @param details Any additional human readable details
#' @param subclass Use if you want to further customise the class
#' @param ...,message,.subclass Only use these fields when creating a subclass.
#'
#' @section Lossy cast errors:
#'
#' By default, lossy casts are an error. Use `allow_lossy_cast()` to
#' silence these errors and continue with the partial results. In this
#' case the lost values are typically set to `NA` or to a lower value
#' resolution, depending on the type of cast.
#'
#' Lossy cast errors are thrown by `maybe_lossy_cast()`. Unlike
#' functions prefixed with `stop_`, `maybe_lossy_cast()` usually
#' returns a result. If a lossy cast is detected, it throws an error,
#' unless it's been wrapped in `allow_lossy_cast()`. In that case, it
#' returns the result silently.
#'
#' @examples
#'
#' # Most of the time, `maybe_lossy_cast()` returns its input normally:
#' maybe_lossy_cast(c("foo", "bar"), NULL, "", lossy = c(FALSE, FALSE))
#'
#' # If `lossy` has any `TRUE`, an error is thrown:
#' try(maybe_lossy_cast(c("foo", "bar"), NULL, "", lossy = c(FALSE, TRUE)))
#'
#' # Unless lossy casts are allowed:
#' allow_lossy_cast(
#'   maybe_lossy_cast(c("foo", "bar"), NULL, "", lossy = c(FALSE, TRUE))
#' )
#'
#' @keywords internal
#' @name vctrs-conditions
NULL

stop_vctrs <- function(message = NULL, .subclass = NULL, ...) {
  abort(message, .subclass = c(.subclass, "vctrs_error"), ...)
}

stop_incompatible <- function(x, y, details = NULL, ..., message = NULL, .subclass = NULL) {
  stop_vctrs(
    message,
    .subclass = c(.subclass, "vctrs_error_incompatible"),
    x = x,
    y = y,
    details = details,
    ...
  )
}

#' @return
#' `stop_incompatible_*()` unconditionally raise an error of class
#' `"vctrs_error_incompatible_*"` and `"vctrs_error_incompatible"`.
#'
#' @rdname vctrs-conditions
#' @export
stop_incompatible_type <- function(x, y,
                                   x_arg = "",
                                   y_arg = "",
                                   details = NULL,
                                   ...,
                                   message = NULL,
                                   .subclass = NULL) {
  vec_assert(x)
  vec_assert(y)

  if (is_null(message)) {
    if (nzchar(x_arg)) {
      x_name <- paste0(" `", x_arg, "` ")
    } else {
      x_name <- " "
    }
    if (nzchar(y_arg)) {
      y_name <- paste0(" `", y_arg, "` ")
    } else {
      y_name <- " "
    }

    message <- glue_lines(
      "No common type for{x_name}<{vec_ptype_full(x)}> and{y_name}<{vec_ptype_full(y)}>.",
      details
    )
  }

  stop_incompatible(
    x, y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    ...,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_type")
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_cast <- function(x,
                                   y,
                                   details = NULL,
                                   ...,
                                   x_arg = "",
                                   to_arg = "",
                                   message = NULL,
                                   .subclass = NULL) {
  if (is_null(message)) {
    x_label <- format_arg_label(vec_ptype_full(x), x_arg)
    to_label <- format_arg_label(vec_ptype_full(y), to_arg)

    message <- glue_lines(
      "Can't cast {x_label} to {to_label}.",
      details
    )
  }

  stop_incompatible(
    x, y,
    details = details,
    ...,
    x_arg = x_arg,
    y_arg = to_arg,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_cast")
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_op <- function(op, x, y, details = NULL, ..., message = NULL, .subclass = NULL) {

  message <- message %||% glue_lines(
    "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}> is not permitted",
    details
  )

  stop_incompatible(
    x, y,
    op = op,
    details = details,
    ...,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_op")
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_size <- function(x, y,
                                   x_size, y_size,
                                   x_arg = "", y_arg = "",
                                   details = NULL,
                                   ...,
                                   message = NULL,
                                   .subclass = NULL) {
  vec_assert(x)
  vec_assert(y)

  vec_assert(x_size, int(), 1)
  vec_assert(y_size, int(), 1)

  if (is_null(message)) {
    if (nzchar(x_arg)) {
      x_name <- paste0("`", x_arg, "`, size")
    } else {
      x_name <- "vector, size"
    }
    if (nzchar(y_arg)) {
      y_name <- paste0("`", y_arg, "`, size")
    } else {
      y_name <- "vector, size"
    }

    message <- glue_lines(
      "No common size for {x_name} {x_size}, and {y_name} {y_size}.",
      details
    )
  }

  stop_incompatible(
    x, y,
    x_size = x_size,
    y_size = y_size,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    ...,
    message = message,
    .subclass = c(.subclass, "vctrs_error_incompatible_size")
  )
}

#' @rdname vctrs-conditions
#' @param result The result of a potentially lossy cast.
#' @param to Type to cast to.
#' @param lossy A logical vector indicating which elements of `result`
#'   were lossy.
#'
#'   Can also be a single `TRUE`, but note that `locations` picks up
#'   locations from this vector by default. In this case, supply your
#'   own location vector, possibly empty.
#' @param locations An optional integer vector giving the
#'   locations where `x` lost information.
#' @param .deprecation If `TRUE`, the error is downgraded to a
#'   deprecation warning. This is useful for transitioning your class
#'   to a stricter conversion scheme. The warning advises your users
#'   to wrap their code with `allow_lossy_cast()`.
#' @export
maybe_lossy_cast <- function(result, x, to,
                             lossy = NULL,
                             locations = NULL,
                             details = NULL,
                             ...,
                             x_arg = "",
                             to_arg = "",
                             message = NULL,
                             .subclass = NULL,
                             .deprecation = FALSE) {
  if (!any(lossy)) {
    return(result)
  }
  if (.deprecation) {
    maybe_warn_deprecated_lossy_cast(x, to, x_arg, to_arg)
    return(result)
  }

  locations <- locations %||% which(lossy)

  withRestarts(
    vctrs_restart_error_cast_lossy = function() result,
    stop_lossy_cast(
      x = x,
      to = to,
      result = result,
      locations = locations,
      details = details,
      ...,
      x_arg = x_arg,
      to_arg = to_arg,
      message = message,
      .subclass = .subclass
    )
  )
}
stop_lossy_cast <- function(x, to, result,
                            locations = NULL,
                            details = NULL,
                            ...,
                            x_arg = "",
                            to_arg = "",
                            message = NULL,
                            .subclass = NULL) {
  if (length(locations)) {
    locations <- inline_list("Locations: ", locations)
  }

  if (is_null(message)) {
    x_label <- format_arg_label(vec_ptype_full(x), x_arg)
    to_label <- format_arg_label(vec_ptype_full(to), to_arg)

    message <- glue_lines(
      "Lossy cast from {x_label} to {to_label}.",
      locations,
      details
    )
  }

  stop_vctrs(
    message,
    x = x,
    y = to,
    to = to,
    result = result,
    locations = locations,
    details = details,
    ...,
    .subclass = c(.subclass, "vctrs_error_cast_lossy")
  )
}
#' @rdname vctrs-conditions
#' @param x_ptype,to_ptype Suppress only the casting errors where `x`
#'   or `to` match these [prototypes][vec_ptype].
#' @export
allow_lossy_cast <- function(expr, x_ptype = NULL, to_ptype = NULL) {
  withCallingHandlers(
    vctrs_error_cast_lossy = function(err) {
      if (!is_null(x_ptype) && !vec_is(err$x, x_ptype)) {
        return()
      }
      if (!is_null(to_ptype) && !vec_is(err$to, to_ptype)) {
        return()
      }

      invokeRestart("vctrs_restart_error_cast_lossy")
    },
    expr
  )
}

maybe_warn_deprecated_lossy_cast <- function(x, to, x_arg, to_arg) {
  # Returns `TRUE` if `allow_lossy_cast()` is on the stack and accepts
  # to handle the condition
  handled <- withRestarts(
    vctrs_restart_error_cast_lossy = function() TRUE,
    {
      cnd_signal(cnd("vctrs_error_cast_lossy", x = x, to = to))
      FALSE
    }
  )

  if (handled) {
    return(invisible())
  }

  from <- format_arg_label(vec_ptype_abbr(x), x_arg)
  to <- format_arg_label(vec_ptype_abbr(to), to_arg)

  warn_deprecated(paste_line(
    glue::glue("We detected a lossy transformation from `{ from }` to `{ to }`."),
    "The result will contain lower-resolution values or missing values.",
    "To suppress this warning, wrap your code with `allow_lossy_cast()`:",
    "",
    "  # Allow all lossy transformations:",
    "  vctrs::allow_lossy_cast(mycode())",
    "",
    "  # Allow only a specific transformation:",
    "  vctrs::allow_lossy_cast(mycode(), x_ptype = from, to_ptype = to)",
    "",
    "Consult `?vctrs::allow_lossy_cast` for more information."
  ))

  invisible()
}

stop_unsupported <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not supported.")
  stop_vctrs(
    "vctrs_error_unsupported",
    message = msg,
    x = x,
    method = method
  )
}

stop_unimplemented <- function(x, method) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not implemented.")
  stop_vctrs(
    "vctrs_error_unimplemented",
    message = msg,
    x = x,
    method = method
  )
}

stop_scalar_type <- function(x, arg = NULL) {
  if (is_null(arg) || !nzchar(arg)) {
    msg <- glue::glue("Expected a vector, not { friendly_type_of(x) }")
  } else {
    msg <- glue::glue("`{ arg }` must be a vector, not { friendly_type_of(x) }")
  }
  stop_vctrs(msg, "vctrs_error_scalar_type", actual = x)
}


# Names -------------------------------------------------------------------

stop_names <- function(message, .subclass, locations, ...) {
  stop_vctrs(
    message,
    .subclass = c(.subclass, "vctrs_error_names"),
    locations = locations,
    ...
  )
}

stop_names_cannot_be_empty <- function(locations) {
  stop_names(
    "Names must not be empty.",
    .subclass = "vctrs_error_names_cannot_be_empty",
    locations = locations
  )
}

stop_names_cannot_be_dot_dot <- function(locations) {
  stop_names(
    "Names must not be of the form `...` or `..j`.",
    .subclass = "vctrs_error_names_cannot_be_dot_dot",
    locations = locations
  )
}

stop_names_must_be_unique <- function(locations) {
  stop_names(
    "Names must be unique.",
    .subclass = "vctrs_error_names_must_be_unique",
    locations = locations
  )
}


# Helpers -----------------------------------------------------------------

glue_lines <- function(..., env = parent.frame()) {
  lines <- c(...)
  out <- map_chr(lines, glue::glue, .envir = env)
  paste(out, collapse = "\n")
}

format_arg_label <- function(type, arg = "") {
  type <- paste0("<", type, ">")
  if (nzchar(arg)) {
    paste0("`", arg, "` ", type)
  } else {
    type
  }
}
