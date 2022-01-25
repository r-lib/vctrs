str_dup <- function(x, times) {
  paste0(rep(x, times = times), collapse = "")
}

indent <- function(x, n) {
  pad <- str_dup(" ", n)
  map_chr(x, gsub, pattern = "(\n+)", replacement = paste0("\\1", pad))
}

ones <- function(...) {
  array(1, dim = c(...))
}

vec_coerce_bare <- function(x, type) {
  # Unexported wrapper around Rf_coerceVector()
  coerce <- env_get(ns_env("rlang"), "vec_coerce")
  coerce(x, type)
}


# Matches the semantics of c() - based on experimenting with the output
# of c(), not reading the source code.
outer_names <- function(names, outer, n) {
  .Call(vctrs_outer_names, names, outer, vec_cast(n, int()))
}

has_inner_names <- function(x) {
  !all(map_lgl(map(x, vec_names), is.null))
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}


set_partition <- function(x, y) {
  list(
    both = intersect(x, y),
    only_x = setdiff(x, y),
    only_y = setdiff(y, x)
  )
}

all_equal <- function(x) all(x == x[[1]])

inline_list <- function(title, x, width = getOption("width"), quote = "") {
  label_width <- width - nchar(title)
  x <- glue::glue_collapse(
    encodeString(x, quote = quote),
    sep = ", ",
    width = label_width
  )
  paste0(title, x)
}

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

compact <- function(x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}

paste_line <- function (...) {
  paste(chr(...), collapse = "\n")
}

# Experimental
result <- function(ok = NULL, err = NULL) {
  structure(
    list(ok = ok, err = err),
    class = "rlang_result"
  )
}
result_get <- function(x) {
  if (!is_null(x$err)) {
    cnd_signal(x$err)
  }
  x$ok
}

obj_type <- function(x) {
  if (vec_is(x)) {
    vec_ptype_full(x)
  } else if (is.object(x)) {
    paste(class(x), collapse = "/")
  } else if (is_function(x)) {
    "function"
  } else {
    typeof(x)
  }
}

new_opts <- function(x, opts, subclass = NULL, arg = NULL) {
  if (!all(x %in% opts)) {
    if (is_null(arg)) {
      arg <- "Argument"
    } else {
      arg <- glue::glue("`{arg}`")
    }
    opts <- encodeString(opts, quote = "\"")
    opts <- glue::glue_collapse(opts, sep = ", ", last = " or ")
    abort(glue::glue("{arg} must be one of {opts}."))
  }

  structure(
    set_names(opts %in% x, opts),
    class = c(subclass, "vctrs_opts")
  )
}

glue_data_bullets <- function(.data, ..., .env = caller_env()) {
  glue_data <- function(...) glue::glue_data(.data, ..., .envir = .env)
  format_error_bullets(map_chr(chr(...), glue_data))
}

unstructure <- function(x) {
  attributes(x) <- NULL
  x
}

# We almost never want `stringsAsFactors = TRUE`, and `FALSE` became
# the default in R 4.0.0. This wrapper ensures that our tests are compliant
# with versions of R before and after this change. Keeping it in `utils.R`
# rather than as a testthat helper ensures that it is sourced before any other
# testthat helpers.
data.frame <- function(..., stringsAsFactors = NULL) {
  stringsAsFactors <- stringsAsFactors %||% FALSE
  base::data.frame(..., stringsAsFactors = stringsAsFactors)
}

try_catch_callback <- function(data, cnd) {
  .Call(vctrs_try_catch_callback, data, cnd)
}

try_catch_hnd <- function(data) {
  function(cnd) {
    try_catch_callback(data, cnd)
  }
}
try_catch_impl <- function(data, ...) {
  tryCatch(
    try_catch_callback(data, NULL),
    ...
  )
}

ns_methods <- function(name) {
  ns_env(name)$.__S3MethodsTable__.
}

s3_find_method <- function(x, generic, ns = "base") {
  stopifnot(
    is_string(generic),
    is_string(ns)
  )
  table <- ns_methods(ns_env(ns))
  .Call(vctrs_s3_find_method, generic, x, table)
}

df_has_base_subset <- function(x) {
  method <- s3_find_method(x, "[", ns = "base")
  is_null(method) || identical(method, `[.data.frame`)
}

last <- function(x) {
  x[[length(x)]]
}

# Find the longest common suffix of two vectors
vec_common_suffix <- function(x, y) {
  common <- vec_cast_common(x = x, y = y)
  x <- common$x
  y <- common$y

  x_size <- vec_size(x)
  y_size <- vec_size(y)
  n <- min(x_size, y_size)

  if (!n) {
    return(vec_slice(x, int()))
  }

  # Truncate the start of the vectors so they have equal size
  if (x_size < y_size) {
    y <- vec_slice(y, seq2(y_size - x_size + 1, y_size))
  } else if (y_size < x_size) {
    x <- vec_slice(x, seq2(x_size - y_size + 1, x_size))
  }

  # Find locations of unequal elements. Elements after the last
  # location are the common suffix.
  common <- vec_equal(x, y)
  i <- which(!common)

  # Slice the suffix after the last unequal element
  if (length(i)) {
    vec_slice(x, seq2(max(i) + 1, n))
  } else {
    x
  }
}

import_from <- function(ns, names, env = caller_env()) {
  objs <- env_get_list(ns_env(ns), names)
  env_bind(env, !!!objs)
}

fast_c <- function(x, y) {
  .Call(vctrs_fast_c, x, y)
}

# Based on r-lib/bench (itself based on gaborcsardi/prettyunits)
#' @export
format.vctrs_bytes <- function(x, scientific = FALSE, digits = 3, drop0trailing = TRUE, ...) {
  nms <- names(x)

  bytes <- unclass(x)

  unit <- map_chr(x, find_unit, byte_units)
  res <- round(bytes / byte_units[unit], digits = digits)

  ## Zero bytes
  res[bytes == 0] <- 0
  unit[bytes == 0] <- "B"

  ## NA and NaN bytes
  res[is.na(bytes)] <- NA_real_
  res[is.nan(bytes)] <- NaN
  unit[is.na(bytes)] <- ""            # Includes NaN as well

  # Append an extra B to each unit
  large_units <- unit %in% names(byte_units)[-1]
  unit[large_units] <- paste0(unit[large_units], "B")

  res <- format(res, scientific = scientific, digits = digits, drop0trailing = drop0trailing, ...)

  stats::setNames(paste0(res, unit), nms)
}
#' @export
print.vctrs_bytes <- function(x, ...) {
  print(format(x, ...), quote = FALSE)
}

tolerance <- sqrt(.Machine$double.eps)
find_unit <- function(x, units) {
  if (is.na(x) || is.nan(x) || x <= 0 || is.infinite(x)) {
    return(NA_character_)
  }
  epsilon <- 1 - (x * (1 / units))
  names(utils::tail(n = 1, which(epsilon < tolerance)))
}

byte_units <- c(
  'B' = 1,
  'K' = 1024,
  'M' = 1024 ^ 2,
  'G' = 1024 ^ 3,
  'T' = 1024 ^ 4,
  'P' = 1024 ^ 5,
  'E' = 1024 ^ 6,
  'Z' = 1024 ^ 7,
  'Y' = 1024 ^ 8
)

new_vctrs_bytes <- function(x) {
  structure(x, class = c("vctrs_bytes", "numeric"))
}

named <- function(x) {
  if (is_null(names(x))) {
    names(x) <- names2(x)
  }
  x
}

browser <- function(...,
                    skipCalls = 0,
                    frame = parent.frame()) {
  if (!identical(stdout(), getConnection(1))) {
    sink(getConnection(1))
    withr::defer(sink(), envir = frame)
  }

  # Calling `browser()` on exit avoids RStudio displaying the
  # `browser2()` location. We still need one `n` to get to the
  # expected place. Ideally `skipCalls` would not skip but exit the
  # contexts.
  on.exit(base::browser(..., skipCalls = skipCalls + 1))
}
