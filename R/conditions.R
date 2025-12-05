#' Custom conditions for vctrs package
#'
#' These functions are called for their side effect of raising
#' errors and warnings.
#' These conditions have custom classes and structures to make
#' testing easier.
#'
#' @inheritParams rlang::args_error_context
#' @param x,y,to Vectors
#' @param ...,class Only use these fields when creating a subclass.
#' @param x_arg,y_arg,to_arg Argument names for `x`, `y`, and `to`. Used in
#'   error messages to inform the user about the locations of incompatible
#'   types.
#' @param action An option to customize the incompatible type message depending
#'   on the context. Errors thrown from [vec_ptype2()] use `"combine"` and
#'   those thrown from [vec_cast()] use `"convert"`.
#' @param details Any additional human readable details.
#' @param message An overriding message for the error. `details` and
#'   `message` are mutually exclusive, supplying both is an error.
#'
#' @examples
#'
#' # Most of the time, `maybe_lossy_cast()` returns its input normally:
#' maybe_lossy_cast(
#'   c("foo", "bar"),
#'   NA,
#'   "",
#'   lossy = c(FALSE, FALSE),
#'   x_arg = "",
#'   to_arg = ""
#' )
#'
#' # If `lossy` has any `TRUE`, an error is thrown:
#' try(maybe_lossy_cast(
#'   c("foo", "bar"),
#'   NA,
#'   "",
#'   lossy = c(FALSE, TRUE),
#'   x_arg = "",
#'   to_arg = ""
#' ))
#'
#' # Unless lossy casts are allowed:
#' allow_lossy_cast(
#'   maybe_lossy_cast(
#'     c("foo", "bar"),
#'     NA,
#'     "",
#'     lossy = c(FALSE, TRUE),
#'     x_arg = "",
#'     to_arg = ""
#'   )
#' )
#'
#' @keywords internal
#' @name vctrs-conditions
NULL

stop_vctrs <- function(
  message = NULL,
  class = NULL,
  ...,
  call = caller_env()
) {
  abort(
    message,
    class = c(class, "vctrs_error"),
    ...,
    call = call
  )
}
warn_vctrs <- function(
  message = NULL,
  class = NULL,
  ...,
  call = caller_env()
) {
  warn(
    message,
    class = c(class, "vctrs_warning"),
    ...,
    call = call
  )
}

stop_incompatible <- function(
  x,
  y,
  ...,
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
) {
  stop_vctrs(
    message,
    class = c(class, "vctrs_error_incompatible"),
    x = x,
    y = y,
    details = details,
    ...,
    call = call
  )
}

#' @return
#' `stop_incompatible_*()` unconditionally raise an error of class
#' `"vctrs_error_incompatible_*"` and `"vctrs_error_incompatible"`.
#'
#' @rdname vctrs-conditions
#' @export
stop_incompatible_type <- function(
  x,
  y,
  ...,
  x_arg,
  y_arg,
  action = c("combine", "convert"),
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
) {
  obj_check_vector(x, arg = x_arg)
  obj_check_vector(y, arg = y_arg)

  action <- arg_match(action)

  message <- cnd_type_message(
    x,
    y,
    x_arg,
    y_arg,
    details,
    action,
    message,
    from_dispatch = match_from_dispatch(...)
  )

  subclass <- switch(
    action,
    combine = "vctrs_error_ptype2",
    convert = "vctrs_error_cast"
  )

  stop_incompatible(
    x,
    y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    ...,
    message = message,
    class = c(class, subclass, "vctrs_error_incompatible_type"),
    call = call
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_cast <- function(
  x,
  to,
  ...,
  x_arg,
  to_arg,
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
) {
  stop_incompatible_type(
    x = x,
    y = to,
    to = to,
    ...,
    x_arg = x_arg,
    y_arg = to_arg,
    to_arg = to_arg,
    action = "convert",
    details = details,
    message = message,
    class = class,
    call = call
  )
}

stop_incompatible_shape <- function(
  x,
  y,
  x_size,
  y_size,
  axis,
  x_arg,
  y_arg,
  call = caller_env()
) {
  details <- format_error_bullets(c(
    x = glue::glue(
      "Incompatible sizes {x_size} and {y_size} along axis {axis}."
    )
  ))
  stop_incompatible_type(
    x,
    y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    call = call
  )
}

type_actions <- c(
  "combine",
  "convert"
)

cnd_type_separator <- function(action) {
  if (identical(action, "combine")) {
    "and"
  } else if (identical(action, "convert")) {
    "to"
  } else {
    abort("Internal error: Unknown `action`.")
  }
}

cnd_type_message <- function(
  x,
  y,
  x_arg,
  y_arg,
  details,
  action,
  message,
  from_dispatch = FALSE,
  fallback = NULL
) {
  if (!is_null(message)) {
    if (!is_null(details)) {
      abort("Can't supply both `message` and `details`.")
    }
    return(message)
  }

  x_arg <- arg_as_string(x_arg)
  y_arg <- arg_as_string(y_arg)

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

  separator <- cnd_type_separator(action)

  if (is.data.frame(x) && is.data.frame(y)) {
    if (vec_is_coercible(new_data_frame(x), new_data_frame(y))) {
      x_type <- cnd_type_message_df_label(x)
      y_type <- cnd_type_message_df_label(y)
    } else {
      x_type <- vec_ptype_full(x)
      y_type <- vec_ptype_full(y)
    }
  } else {
    x_type <- cnd_type_message_type_label(x)
    y_type <- cnd_type_message_type_label(y)
  }

  converting <- action == "convert"

  # If we are here directly from dispatch, this means there is no
  # ptype2 method implemented and the is-same-class fallback has
  # failed because of diverging attributes. The author of the class
  # should implement a ptype2 method as documented in the FAQ
  # indicated below.
  if (from_dispatch && !converting && identical(class(x)[[1]], class(y)[[1]])) {
    details <- c(incompatible_attrib_bullets(), details)
    details <- format_error_bullets(details)
  }

  if (is_null(fallback)) {
    end <- "."
  } else {
    end <- glue::glue("; falling back to {fallback}.")
  }

  if (converting && nzchar(y_arg)) {
    header <- glue::glue(
      "Can't convert{x_name}<{x_type}> to match type of{y_name}<{y_type}>{end}"
    )
  } else {
    header <- glue::glue(
      "Can't {action}{x_name}<{x_type}> {separator}{y_name}<{y_type}>{end}"
    )
  }

  paste_line(header, details)
}

cnd_type_message_type_label <- function(x) {
  if (is.data.frame(x)) {
    class(x)[[1]]
  } else {
    vec_ptype_full(x)
  }
}
incompatible_attrib_bullets <- function() {
  c(
    x = "Some attributes are incompatible.",
    i = "The author of the class should implement vctrs methods.",
    i = "See <https://vctrs.r-lib.org/reference/faq-error-incompatible-attributes.html>."
  )
}

cnd_type_message_df_label <- function(x) {
  x <- class(x)[[1]]

  if (identical(x, "tbl_df")) {
    "tibble"
  } else {
    x
  }
}


#' @rdname vctrs-conditions
#' @export
stop_incompatible_op <- function(
  op,
  x,
  y,
  details = NULL,
  ...,
  message = NULL,
  class = NULL,
  call = caller_env()
) {
  message <- message %||%
    glue_lines(
      "<{vec_ptype_full(x)}> {op} <{vec_ptype_full(y)}> is not permitted",
      details
    )

  stop_incompatible(
    x,
    y,
    op = op,
    details = details,
    ...,
    message = message,
    class = c(class, "vctrs_error_incompatible_op"),
    call = call
  )
}

#' @rdname vctrs-conditions
#' @export
stop_incompatible_size <- function(
  x,
  y,
  x_size,
  y_size,
  ...,
  x_arg,
  y_arg,
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
) {
  stop_incompatible(
    x,
    y,
    x_size = x_size,
    y_size = y_size,
    ...,
    x_arg = x_arg,
    y_arg = y_arg,
    details = details,
    message = message,
    class = c(class, "vctrs_error_incompatible_size"),
    call = call
  )
}

#' @export
cnd_header.vctrs_error_incompatible_size <- function(cnd, ...) {
  if (is_string(cnd$message) && nzchar(cnd$message)) {
    return(cnd$message)
  }

  x_size <- vec_cast(cnd$x_size, int())
  y_size <- vec_cast(cnd$y_size, int())

  stopifnot(
    length(x_size) == 1,
    length(y_size) == 1
  )

  x_arg <- arg_as_string(cnd$x_arg)
  y_arg <- arg_as_string(cnd$y_arg)

  if (nzchar(x_arg)) {
    x_tag <- glue::glue("`{x_arg}` (size {x_size})")
  } else {
    x_tag <- glue::glue("input of size {x_size}")
  }
  if (nzchar(y_arg)) {
    y_tag <- glue::glue("to match `{y_arg}` (size {y_size})")
  } else {
    y_tag <- glue::glue("to size {y_size}")
  }

  glue::glue("Can't recycle {x_tag} {y_tag}.")
}

#' @export
cnd_body.vctrs_error_incompatible_size <- function(cnd, ...) {
  cnd$details
}

#' Lossy cast error
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
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
#' @inheritParams stop_incompatible_cast
#' @inheritParams vec_cast
#' @inheritParams rlang::args_error_context
#' @param result The result of a potentially lossy cast.
#' @param to Type to cast to.
#' @param lossy A logical vector indicating which elements of `result`
#'   were lossy.
#'
#'   Can also be a single `TRUE`, but note that `locations` picks up
#'   locations from this vector by default. In this case, supply your
#'   own location vector, possibly empty.
#' @param loss_type The kind of lossy cast to be mentioned in error
#'   messages. Can be loss of precision (for instance from double to
#'   integer) or loss of generality (from character to factor).
#' @param locations An optional integer vector giving the
#'   locations where `x` lost information.
#' @param .deprecation If `TRUE`, the error is downgraded to a
#'   deprecation warning. This is useful for transitioning your class
#'   to a stricter conversion scheme. The warning advises your users
#'   to wrap their code with `allow_lossy_cast()`.
#' @keywords internal
#' @export
maybe_lossy_cast <- function(
  result,
  x,
  to,
  lossy = NULL,
  locations = NULL,
  ...,
  loss_type = c("precision", "generality"),
  x_arg,
  to_arg,
  call = caller_env(),
  details = NULL,
  message = NULL,
  class = NULL,
  .deprecation = FALSE
) {
  if (!any(lossy)) {
    return(result)
  }
  if (.deprecation) {
    maybe_warn_deprecated_lossy_cast(x, to, loss_type, x_arg, to_arg)
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
      ...,
      loss_type = loss_type,
      x_arg = x_arg,
      to_arg = to_arg,
      details = details,
      message = message,
      class = class,
      call = call
    )
  )
}
stop_lossy_cast <- function(
  x,
  to,
  result,
  locations = NULL,
  ...,
  loss_type,
  x_arg,
  to_arg,
  details = NULL,
  message = NULL,
  class = NULL,
  call = caller_env()
) {
  stop_incompatible_cast(
    x = x,
    to = to,
    result = result,
    locations = locations,
    ...,
    loss_type = loss_type,
    x_arg = x_arg,
    to_arg = to_arg,
    details = details,
    class = c(class, "vctrs_error_cast_lossy"),
    call = call
  )
}

#' @export
cnd_header.vctrs_error_cast_lossy <- function(cnd, ...) {
  x_label <- format_arg_label(vec_ptype_full(cnd$x), cnd$x_arg)
  to_label <- format_arg_label(vec_ptype_full(cnd$y), cnd$y_arg)
  loss_type <- loss_type(cnd$loss_type)
  glue::glue(
    "Can't convert from {x_label} to {to_label} due to loss of {loss_type}."
  )
}
#' @export
cnd_body.vctrs_error_cast_lossy <- function(cnd, ...) {
  if (length(cnd$locations)) {
    format_error_bullets(inline_list("Locations: ", cnd$locations))
  } else {
    character()
  }
}

loss_type <- function(x) {
  stopifnot(
    is_character(x),
    all(x %in% c("precision", "generality"))
  )
  x[[1]]
}

# Used in maybe_warn_deprecated_lossy_cast()
new_error_cast_lossy <- function(x, to, loss_type, x_arg = "", to_arg = "") {
  error_cnd(
    c("vctrs_error_cast_lossy", "vctrs_error_incompatible_type"),
    x = x,
    y = to,
    loss_type = loss_type,
    x_arg = x_arg,
    y_arg = to_arg
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
      if (!is_null(to_ptype) && !vec_is(err$y, to_ptype)) {
        return()
      }

      invokeRestart("vctrs_restart_error_cast_lossy")
    },
    expr
  )
}

maybe_warn_deprecated_lossy_cast <- function(
  x,
  to,
  loss_type,
  x_arg,
  to_arg,
  user_env = caller_env(2)
) {
  # Returns `TRUE` if `allow_lossy_cast()` is on the stack and accepts
  # to handle the condition
  handled <- withRestarts(
    vctrs_restart_error_cast_lossy = function() TRUE,
    {
      # Signal fully formed condition but strip the error classes in
      # case someone is catching: This is not an abortive condition.
      cnd <- new_error_cast_lossy(
        x,
        to,
        loss_type = loss_type,
        x_arg = x_arg,
        to_arg = to_arg
      )

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

  lifecycle::deprecate_warn(
    when = "0.2.0",
    what = I("Coercion with lossy casts"),
    with = "allow_lossy_cast()",
    details = paste0(
      glue::glue(
        "We detected a lossy transformation from { from } to { to }. "
      ),
      "The result will contain lower-resolution values or missing values. ",
      "To suppress this warning, wrap your code with `allow_lossy_cast()`."
    ),
    always = TRUE,
    user_env = user_env
  )

  invisible()
}

stop_unsupported <- function(x, method, call = caller_env()) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not supported.")
  stop_vctrs(
    "vctrs_error_unsupported",
    message = msg,
    x = x,
    method = method,
    call = call
  )
}

stop_unimplemented <- function(x, method, call = caller_env()) {
  msg <- glue::glue("`{method}.{class(x)[[1]]}()` not implemented.")
  stop_vctrs(
    "vctrs_error_unimplemented",
    message = msg,
    x = x,
    method = method,
    call = call
  )
}

stop_scalar_type <- function(x, arg = NULL, call = caller_env()) {
  if (is_null(arg) || !nzchar(arg)) {
    arg <- "Input"
  } else {
    arg <- glue::backtick(arg)
  }

  message <- glue::glue("{arg} must be a vector, not {obj_type_friendly(x)}.")

  # Use the first detected issue, with a fallthrough to point to our scalar FAQ
  message <-
    with_incompatible_s3_list_bullets(message, x) %||%
    with_incompatible_data_frame_bullets(message, x) %||%
    with_scalar_faq_bullet(message)

  stop_vctrs(
    message,
    "vctrs_error_scalar_type",
    actual = x,
    call = call
  )
}

with_incompatible_s3_list_bullets <- function(message, x) {
  is_list_typeof <- typeof(x) == "list"

  classes <- class(x)
  doesnt_contain_explicit_list_class <- !any(classes == "list")
  doesnt_contain_data_frame_class <- !any(classes == "data.frame")

  # We also assume no `vec_proxy()` method exists, otherwise one would have
  # been invoked, avoiding the error
  is_incompatible_s3_list <-
    is_list_typeof &&
    doesnt_contain_explicit_list_class &&
    doesnt_contain_data_frame_class

  if (!is_incompatible_s3_list) {
    return(NULL)
  }

  c(
    message,
    x = cli::format_inline(paste(
      "Detected incompatible scalar S3 list.",
      "To be treated as a vector, the object must explicitly inherit from {.cls list}",
      "or should implement a {.fn vec_proxy} method.",
      "Class: {.cls {classes}}."
    )),
    i = "If this object comes from a package, please report this error to the package author.",
    i = cli::format_inline(paste(
      "Read our FAQ about",
      "{.topic [creating vector types](vctrs::howto_faq_fix_scalar_type_error)}",
      "to learn more."
    ))
  )
}

with_incompatible_data_frame_bullets <- function(message, x) {
  classes <- class(x)
  n_classes <- length(classes)

  contains_data_frame_class <- any(classes == "data.frame")

  if (n_classes == 0L) {
    # Edge case of `NULL` or `character()` classes
    last_class_is_not_data_frame <- TRUE
  } else {
    last_class_is_not_data_frame <- classes[n_classes] != "data.frame"
  }

  is_incompatible_data_frame <-
    contains_data_frame_class && last_class_is_not_data_frame

  if (!is_incompatible_data_frame) {
    return(NULL)
  }

  subclasses <- setdiff(classes, "data.frame")

  c(
    message,
    x = cli::format_inline(paste(
      "Detected incompatible data frame structure.",
      "A data frame is normally treated as a vector, but an incompatible class ordering was detected.",
      "To be compatible, the subclass {.cls {subclasses}} must come before {.cls data.frame}, not after.",
      "Class: {.cls {classes}}."
    )),
    i = "If this object comes from a package, please report this error to the package author.",
    i = cli::format_inline(paste(
      "Read our FAQ about",
      "{.topic [creating vector types](vctrs::howto_faq_fix_scalar_type_error)}",
      "to learn more."
    ))
  )
}

with_scalar_faq_bullet <- function(message) {
  c(
    message,
    i = cli::format_inline(paste(
      "Read our FAQ about {.topic [scalar types](vctrs::faq_error_scalar_type)}",
      "to learn more."
    ))
  )
}

stop_corrupt_factor_levels <- function(x, arg = "x", call = caller_env()) {
  msg <- glue::glue("`{arg}` is a corrupt factor with non-character levels")
  abort(msg, call = call)
}

stop_corrupt_ordered_levels <- function(x, arg = "x", call = caller_env()) {
  msg <- glue::glue(
    "`{arg}` is a corrupt ordered factor with non-character levels"
  )
  abort(msg, call = call)
}

stop_recycle_incompatible_size <- function(
  x_size,
  size,
  x_arg = "x",
  call = caller_env()
) {
  stop_vctrs(
    x_size = x_size,
    y_size = size,
    x_arg = x_arg,
    # FIXME: tibble is the only package that uses `vctrs_error_recycle_incompatible_size`
    class = c(
      "vctrs_error_incompatible_size",
      "vctrs_error_recycle_incompatible_size"
    ),
    call = call
  )
}


# Names -------------------------------------------------------------------

stop_names <- function(class = NULL, ..., call = caller_env()) {
  stop_vctrs(
    class = c(class, "vctrs_error_names"),
    ...,
    call = call
  )
}

stop_names_cannot_be_empty <- function(names, call = caller_env()) {
  stop_names(
    class = "vctrs_error_names_cannot_be_empty",
    names = names,
    call = call
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
    bullet <- glue::glue(
      "Empty names found at locations {ensure_full_stop(enumerate(locations))}"
    )
  }

  bullet <- c(x = bullet)
  format_error_bullets(bullet)
}

stop_names_cannot_be_dot_dot <- function(names, call = caller_env()) {
  stop_names(
    class = "vctrs_error_names_cannot_be_dot_dot",
    names = names,
    call = call
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

stop_names_must_be_unique <- function(names, arg = "", call = caller_env()) {
  stop_names(
    class = "vctrs_error_names_must_be_unique",
    arg = arg,
    names = names,
    call = call
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

  arg <- arg_as_string(cnd$arg)
  if (arg != "") {
    hint <- c(
      i = glue::glue("Use argument `{cnd$arg}` to specify repair strategy.")
    )
    message <- c(message, format_error_bullets(hint))
  }

  message
}


make_names_loc_bullet <- function(x, loc) {
  if (length(loc) == 1) {
    glue::glue("{glue::double_quote(x)} at location {loc}.")
  } else {
    glue::glue(
      "{glue::double_quote(x)} at locations {ensure_full_stop(enumerate(loc))}"
    )
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
  cli::cli_abort(
    c(
      "{.fn {fn}} is implemented at C level.",
      " " = "This R function is purely indicative and should never be called."
    ),
    .internal = TRUE
  )
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

arg_backtick <- function(arg, or = "Input") {
  if (nzchar(arg)) {
    glue::backtick(arg)
  } else {
    or
  }
}

arg_as_string <- function(arg) {
  if (is_null(arg)) {
    ""
  } else if (is_string(arg)) {
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
