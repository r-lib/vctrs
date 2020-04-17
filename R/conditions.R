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
stop_incompatible_type <- function(x,
                                   y,
                                   x_arg = "",
                                   y_arg = "",
                                   details = NULL,
                                   ...,
                                   message = NULL,
                                   class = NULL) {
  stop_incompatible_type_combine(
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    ...,
    message = message,
    class = class
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
  stop_incompatible_type_convert(
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = to_arg,
    details = details,
    ...,
    message = message,
    class = class
  )
}

stop_incompatible_shape <- function(x, y, x_size, y_size, axis, x_arg, y_arg) {
  details <- format_error_bullets(c(
    x = glue::glue("Incompatible sizes {x_size} and {y_size} along axis {axis}.")
  ))
  stop_incompatible_type(x, y, x_arg, y_arg, details = details)
}

stop_incompatible_type_convert <- function(x,
                                           y,
                                           x_arg = "",
                                           y_arg = "",
                                           details = NULL,
                                           ...,
                                           message = NULL,
                                           class = NULL) {
  stop_incompatible_type_impl(
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    action = "convert",
    ...,
    message = message,
    class = class
  )
}

stop_incompatible_type_combine <- function(x,
                                           y,
                                           x_arg = "",
                                           y_arg = "",
                                           details = NULL,
                                           ...,
                                           message = NULL,
                                           class = NULL) {
  stop_incompatible_type_impl(
    x = x,
    y = y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    action = "combine",
    ...,
    message = message,
    class = class
  )
}

stop_incompatible_type_impl <- function(x,
                                        y,
                                        x_arg,
                                        y_arg,
                                        details,
                                        action,
                                        ...,
                                        message,
                                        class) {
  vec_assert(x)
  vec_assert(y)

  message <- cnd_type_message(x, y, x_arg, y_arg, details, action, message)

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

type_actions <- c(
  "combine", "convert"
)

cnd_type_action <- function(action) {
  if (!is_string(action, type_actions)) {
    abort(paste0(
      "Internal error: `action` must be either ",
      "`combine` or `convert`."
    ))
  }

  action
}

cnd_type_separator <- function(action) {
  if (action == "combine") {
    "and"
  } else if (action == "convert") {
    "to"
  } else {
    abort("Internal error: Unknown `action`.")
  }
}

cnd_type_message <- function(x,
                             y,
                             x_arg,
                             y_arg,
                             details,
                             action,
                             message,
                             types = NULL) {
  if (!is_null(message)) {
    return(message)
  }

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

  action <- cnd_type_action(action)
  separator <- cnd_type_separator(action)

  if (is_null(types)) {
    x_type <- vec_ptype_full(x)
    y_type <- vec_ptype_full(y)
  } else {
    stopifnot(is_character(types, n = 2))
    x_type <- types[[1]]
    y_type <- types[[2]]
  }

  glue_lines(
    "Can't {action}{x_name}<{x_type}> {separator}{y_name}<{y_type}>.",
    details
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
  glue::glue("Can't convert from {x_label} to {to_label} due to loss of precision.")
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
    arg <- "Input"
  } else {
    arg <- glue::backtick(arg)
  }
  msg <- glue::glue("{arg} must be a vector, not {friendly_type_of(x)}.")
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
  arg <- append_arg("Input", cnd$x_arg)
  glue::glue("{arg} can't be recycled to size {cnd$size}.")
}
#' @export
cnd_body.vctrs_error_recycle_incompatible_size <- function(cnd, ...) {
  if (cnd$size == 1) {
    msg <- "It must be size 1, not {x_size}."
  } else {
    msg <- "It must be size {size} or 1, not {x_size}."
  }
  glue_data_bullets(cnd, x = msg)
}


# Names -------------------------------------------------------------------

stop_names <- function(class = NULL, ...) {
  stop_vctrs(
    class = c(class, "vctrs_error_names"),
    ...
  )
}

stop_names_cannot_be_empty <- function(names) {
  stop_names(
    class = "vctrs_error_names_cannot_be_empty",
    names = names
  )
}

#' @export
cnd_header.vctrs_error_names_cannot_be_empty <- function(cnd, ...) {
  "Names can't be empty."
}

#' @export
cnd_body.vctrs_error_names_cannot_be_empty <- function(cnd, ...) {
  locations <- detect_empty_names(cnd$names)

  if (length(locations) == 1) {
    bullet <- glue::glue("Empty name found at location {locations}.")
  } else {
    bullet <- glue::glue("Empty names found at locations {ensure_full_stop(enumerate(locations))}")
  }

  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

stop_names_cannot_be_dot_dot <- function(names) {
  stop_names(
    class = "vctrs_error_names_cannot_be_dot_dot",
    names = names
  )
}

#' @export
cnd_header.vctrs_error_names_cannot_be_dot_dot <- function(cnd, ...) {
  "Names can't be of the form `...` or `..j`."
}

#' @export
cnd_body.vctrs_error_names_cannot_be_dot_dot <- function(cnd, ...) {
  names <- cnd$names

  locations <- detect_dot_dot(names)
  names <- names[locations]

  split <- vec_group_loc(names)

  info <- map2_chr(split$key, split$loc, make_names_loc_bullet)

  header <- "These names are invalid:"
  header <- c(x = header)
  header <- format_error_bullets(header)

  message <- bullets(info, header = header)
  message <- indent(message, 2)

  message
}

stop_names_must_be_unique <- function(names) {
  stop_names(
    class = "vctrs_error_names_must_be_unique",
    names = names
  )
}

#' @export
cnd_header.vctrs_error_names_must_be_unique <- function(cnd, ...) {
  "Names must be unique."
}

#' @export
cnd_body.vctrs_error_names_must_be_unique <- function(cnd, ...) {
  names <- cnd$names

  dups <- vec_group_loc(names)
  dup_indicator <- map_lgl(dups$loc, function(x) length(x) != 1L)
  dups <- vec_slice(dups, dup_indicator)

  header <- "These names are duplicated:"
  header <- c(x = header)
  header <- format_error_bullets(header)

  info <- map2_chr(dups$key, dups$loc, make_names_loc_bullet)

  message <- bullets(info, header = header)
  message <- indent(message, 2)

  message
}


make_names_loc_bullet <- function(x, loc) {
  if (length(loc) == 1) {
    glue::glue("{glue::double_quote(x)} at location {loc}.")
  } else {
    glue::glue("{glue::double_quote(x)} at locations {ensure_full_stop(enumerate(loc))}")
  }
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


stop_native_implementation <- function(fn) {
  abort(paste_line(
    glue::glue("`{fn}()` is implemented at C level."),
    "This R function is purely indicative and should never be called."
  ))
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
    return(x)
  }

  arg <- arg_as_string(arg)
  if (nzchar(arg)) {
    glue::glue("{x} `{arg}`")
  } else {
    x
  }
}
