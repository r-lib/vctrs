#' Factor/ordered factor S3 class
#'
#' A [factor] is an integer with attribute `levels`, a character vector. There
#' should be one level for each integer between 1 and `max(x)`.
#' An [ordered] factor has the same properties as a factor, but possesses
#' an extra class that marks levels as having a total ordering.
#'
#' These functions help the base factor and ordered factor classes fit in to
#' the vctrs type system by providing constructors, coercion functions,
#' and casting functions. `new_factor()` and `new_ordered()` are low-level
#' constructors - they only check that types, but not values, are valid, so
#' are for expert use only.
#'
#' @param x Integer values which index in to `levels`.
#' @param levels Character vector of labels.
#' @param ...,class Used to for subclasses.
#' @keywords internal
#' @export
new_factor <- function(
  x = integer(),
  levels = character(),
  ...,
  class = character()
) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))

  structure(
    x,
    levels = levels,
    ...,
    class = c(class, "factor")
  )
}

#' @export
#' @rdname new_factor
new_ordered <- function(x = integer(), levels = character()) {
  new_factor(x = x, levels = levels, class = "ordered")
}

#' @export
vec_proxy.factor <- function(x, ...) {
  x
}

#' @export
vec_proxy.ordered <- function(x, ...) {
  x
}

#' @export
vec_restore.factor <- function(x, to, ...) {
  NextMethod()
}

#' @export
vec_restore.ordered <- function(x, to, ...) {
  NextMethod()
}

# Print -------------------------------------------------------------------

#' @export
vec_ptype_full.factor <- function(x, ...) {
  paste0("factor<", hash_label(levels(x)), ">", vec_ptype_shape(x))
}

#' @export
vec_ptype_abbr.factor <- function(x, ...) {
  "fct"
}

#' @export
vec_ptype_full.ordered <- function(x, ...) {
  paste0("ordered<", hash_label(levels(x)), ">", vec_ptype_shape(x))
}

#' @export
vec_ptype_abbr.ordered <- function(x, ...) {
  "ord"
}

# Coerce ------------------------------------------------------------------

#' @rdname new_factor
#' @export vec_ptype2.factor
#' @method vec_ptype2 factor
#' @export
vec_ptype2.factor <- function(x, y, ...) {
  UseMethod("vec_ptype2.factor")
}
#' @export
vec_ptype2.factor.factor <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.factor.factor")
}
#' @export
vec_ptype2.character.factor <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.character.factor")
}
#' @export
vec_ptype2.factor.character <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.factor.character")
}

#' @rdname new_factor
#' @export vec_ptype2.ordered
#' @method vec_ptype2 ordered
#' @export
vec_ptype2.ordered <- function(x, y, ...) {
  UseMethod("vec_ptype2.ordered")
}
#' @export
vec_ptype2.ordered.ordered <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.ordered.ordered")
}
#' @export
vec_ptype2.ordered.character <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.ordered.character")
}
#' @export
vec_ptype2.character.ordered <- function(x, y, ...) {
  stop_native_implementation("vec_ptype2.character.ordered")
}

#' @export
vec_ptype2.ordered.factor <- function(x, y, ...) {
  vec_default_ptype2(x, y, ...)
}
#' @export
vec_ptype2.factor.ordered <- function(x, y, ...) {
  vec_default_ptype2(x, y, ...)
}


# Cast --------------------------------------------------------------------

#' @rdname new_factor
#' @export vec_cast.factor
#' @method vec_cast factor
#' @export
vec_cast.factor <- function(x, to, ...) {
  UseMethod("vec_cast.factor")
}

fct_cast <- function(x, to, ..., call = caller_env()) {
  fct_cast_impl(x, to, ..., ordered = FALSE, call = call)
}

fct_cast_impl <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = "",
  ordered = FALSE,
  call = caller_env()
) {
  if (length(levels(to)) == 0L) {
    levels <- levels(x)
    if (is.null(levels)) {
      exclude <- NA
      levels <- unique(x)
    } else {
      exclude <- NULL
    }
    factor(
      as.character(x),
      levels = levels,
      ordered = ordered,
      exclude = exclude
    )
  } else {
    lossy <- !(x %in% levels(to) | is.na(x))
    out <- factor(
      x,
      levels = levels(to),
      ordered = ordered,
      exclude = NULL
    )
    maybe_lossy_cast(
      out,
      x,
      to,
      lossy,
      loss_type = "generality",
      x_arg = x_arg,
      to_arg = to_arg,
      call = call
    )
  }
}

#' @export
vec_cast.factor.factor <- function(x, to, ...) {
  fct_cast(x, to, ...)
}
#' @export
vec_cast.factor.character <- function(x, to, ...) {
  fct_cast(x, to, ...)
}
#' @export
vec_cast.character.factor <- function(x, to, ...) {
  stop_native_implementation("vec_cast.character.factor")
}

#' @rdname new_factor
#' @export vec_cast.ordered
#' @method vec_cast ordered
#' @export
vec_cast.ordered <- function(x, to, ...) {
  UseMethod("vec_cast.ordered")
}

ord_cast <- function(x, to, ..., call = caller_env()) {
  fct_cast_impl(x, to, ..., ordered = TRUE, call = call)
}

#' @export
vec_cast.ordered.ordered <- function(x, to, ...) {
  ord_cast(x, to, ...)
}

#' @export
vec_cast.ordered.character <- function(x, to, ...) {
  ord_cast(x, to, ...)
}
#' @export
vec_cast.character.ordered <- function(x, to, ...) {
  stop_native_implementation("vec_cast.character.ordered")
}

# Math and arithmetic -----------------------------------------------------

#' @export
vec_math.factor <- function(.fn, .x, ...) {
  stop_unsupported(.x, .fn)
}

#' @export
vec_arith.factor <- function(op, x, y, ...) {
  stop_unsupported(x, op)
}

# Helpers -----------------------------------------------------------------

hash_label <- function(x, length = 5) {
  if (length(x) == 0) {
    ""
  } else {
    # Can't use obj_hash() because it hashes the string pointers
    # for performance, so the values in the test change each time
    substr(rlang::hash(x), 1, length)
  }
}

levels_union <- function(x, y) {
  union(levels(x), levels(y))
}
