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
#' @param ...,message,class Only use these fields when creating a subclass.
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

stop_vctrs <- function(message = NULL, class = NULL, ...) {
  abort(message, class = c(class, "vctrs_error"), ...)
}

stop_incompatible <- function(x, y, details = NULL, ..., message = NULL, class = NULL) {
  stop_vctrs(
    message,
    class = c(class, "vctrs_error_incompatible"),
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
                                   class = NULL) {
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
    class = c(class, "vctrs_error_incompatible_type")
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
                                   class = NULL) {
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
    class = c(class, "vctrs_error_incompatible_cast")
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_op <- function(op, x, y, details = NULL, ..., message = NULL, class = NULL) {

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
    class = c(class, "vctrs_error_incompatible_op")
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
                                   class = NULL) {
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
    class = c(class, "vctrs_error_incompatible_size")
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
                             class = NULL,
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
      class = class
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
                            class = NULL) {
  stop_vctrs(
    message,
    x = x,
    y = to,
    to = to,
    result = result,
    x_arg = x_arg,
    to_arg = to_arg,
    locations = locations,
    details = details,
    ...,
    class = c(class, "vctrs_error_cast_lossy")
  )
}

#' @export
conditionMessage.vctrs_error_cast_lossy <- function(c) {
  # FIXME: Remove `message` argument
  if (is_string(c$message) && nzchar(c$message)) {
    return(c$message)
  }

  # FIXME: Add `cnd_details()`?
  glue_lines(
    cnd_message(c),
    c$details
  )
}
#' @export
cnd_header.vctrs_error_cast_lossy <- function(cnd, ...) {
  x_label <- format_arg_label(vec_ptype_full(cnd$x), cnd$x_arg)
  to_label <- format_arg_label(vec_ptype_full(cnd$to), cnd$to_arg)
  glue::glue("Lossy cast from {x_label} to {to_label}.")
}
#' @export
cnd_body.vctrs_error_cast_lossy <- function(cnd, ...) {
  if (length(cnd$locations)) {
    format_error_bullets(inline_list("Locations: ", cnd$locations))
  } else {
    character()
  }
}

# Used in maybe_warn_deprecated_lossy_cast()
new_error_cast_lossy <- function(x, to, x_arg = "", to_arg = "") {
  error_cnd(
    "vctrs_error_cast_lossy",
    x = x,
    to = to,
    x_arg = x_arg,
    to_arg = to_arg
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
      # Signal fully formed condition but strip the error classes in
      # case someone is catching: This is not an abortive condition.
      cnd <- new_error_cast_lossy(x, to, x_arg = x_arg, to_arg = to_arg)
      class(cnd) <- setdiff(class(cnd), c("error", "rlang_error"))
      signalCondition(cnd)
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

stop_corrupt_factor_levels <- function(x, arg = "x") {
  msg <- glue::glue("`{arg}` is a corrupt factor with non-character levels")
  abort(msg)
}

stop_corrupt_ordered_levels <- function(x, arg = "x") {
  msg <- glue::glue("`{arg}` is a corrupt ordered factor with non-character levels")
  abort(msg)
}

stop_recycle_incompatible_size <- function(x_size, size, x_arg = "x") {
  stop_vctrs(
    x_size = x_size,
    size = size,
    x_arg = x_arg,
    class = "vctrs_error_recycle_incompatible_size"
  )
}

#' @export
cnd_header.vctrs_error_recycle_incompatible_size <- function(cnd, ...) {
  glue::glue_data(cnd, "`{x_arg}` can't be recycled to size {size}.")
}
#' @export
cnd_body.vctrs_error_recycle_incompatible_size <- function(cnd, ...) {
  glue_data_bullets(
    cnd,
    x = "It must be size {size} or 1, not {x_size}.",
  )
}


# Names -------------------------------------------------------------------

stop_names <- function(message, class, locations, ...) {
  stop_vctrs(
    message,
    class = c(class, "vctrs_error_names"),
    locations = locations,
    ...
  )
}

stop_names_cannot_be_empty <- function(locations) {
  stop_names(
    "Names must not be empty.",
    class = "vctrs_error_names_cannot_be_empty",
    locations = locations
  )
}

stop_names_cannot_be_dot_dot <- function(locations) {
  stop_names(
    "Names must not be of the form `...` or `..j`.",
    class = "vctrs_error_names_cannot_be_dot_dot",
    locations = locations
  )
}

stop_names_must_be_unique <- function(locations) {
  stop_names(
    "Names must be unique.",
    class = "vctrs_error_names_must_be_unique",
    locations = locations
  )
}

enumerate <- function(x, max = 5L, allow_empty = FALSE) {
  n <- length(x)

  if (n == 0L && !allow_empty) {
    abort("Internal error: Enumeration can't be empty.")
  }
  if (n > max) {
    paste0(glue::glue_collapse(x[seq2(1, max)], ", "), ", etc.")
  } else {
    if (n == 2) {
      last <- " and "
    } else {
      last <- ", and "
    }
    glue::glue_collapse(x, ", ", last = last)
  }
}

ensure_full_stop <- function(x) {
  n <- nchar(x)
  if (substr(x, n, n) == ".") {
    x
  } else {
    paste0(x, ".")
  }
}

# Helpers -----------------------------------------------------------------

glue_lines <- function(..., env = parent.frame()) {
  out <- map_chr(chr(...), glue::glue, .envir = env)
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

arg_as_string <- function(arg) {
  if (is_string(arg)) {
    arg
  } else {
    as_label(arg)
  }
}
append_arg <- function(x, arg) {
  if (is_null(arg)) {
    x
  } else {
    arg <- arg_as_string(arg)
    glue::glue("{x} `{arg}`")
  }
}
