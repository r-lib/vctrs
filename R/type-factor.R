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
new_factor <- function(x = integer(), levels = character(), ..., class = character()) {
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

# Print -------------------------------------------------------------------

#' @export
vec_ptype_full.factor <- function(x, ...) {
  paste0("factor<", hash_label(levels(x)), ">")
}

#' @export
vec_ptype_abbr.factor <- function(x, ...) {
  "fct"
}

#' @export
vec_ptype_full.ordered <- function(x, ...) {
  paste0("ordered<", hash_label(levels(x)), ">")
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
vec_ptype2.factor <- function(x, y, ...) UseMethod("vec_ptype2.factor", y)
#' @method vec_ptype2.factor default
#' @export
vec_ptype2.factor.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.character factor
#' @export
vec_ptype2.character.factor <- function(x, y, ...) character()
#' @method vec_ptype2.factor character
#' @export
vec_ptype2.factor.character <- function(x, y, ...) character()
#' @method vec_ptype2.factor factor
#' @export
vec_ptype2.factor.factor <- function(x, y, ...) new_factor(levels = levels_union(x, y))

#' @rdname new_factor
#' @export vec_ptype2.ordered
#' @method vec_ptype2 ordered
#' @export
vec_ptype2.ordered <- function(x, y, ...) UseMethod("vec_ptype2.ordered", y)
#' @method vec_ptype2.ordered default
#' @export
vec_ptype2.ordered.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.ordered character
#' @export
vec_ptype2.ordered.character <- function(x, y, ...) character()
#' @method vec_ptype2.character ordered
#' @export
vec_ptype2.character.ordered <- function(x, y, ...) character()
#' @method vec_ptype2.ordered factor
#' @export
vec_ptype2.ordered.factor <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.factor ordered
#' @export
vec_ptype2.factor.ordered <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.ordered ordered
#' @export
vec_ptype2.ordered.ordered <- function(x, y, ...) new_ordered(levels = levels_union(x, y))

# Cast --------------------------------------------------------------------

#' @rdname new_factor
#' @export vec_cast.factor
#' @method vec_cast factor
#' @export
vec_cast.factor <- function(x, to, ...) {
  UseMethod("vec_cast.factor")
}

#' @export
#' @method vec_cast.factor factor
vec_cast.factor.factor <- function(x, to, ..., x_arg = "", to_arg = "") {
  if (length(levels(to)) == 0L) {
    factor(as.character(x), levels = unique(x), ordered = is.ordered(to))
  } else {
    lossy <- !(x %in% levels(to) | is.na(x))
    out <- factor(x, levels = levels(to), ordered = is.ordered(to))
    maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
  }
}
#' @export
#' @method vec_cast.factor character
vec_cast.factor.character <- vec_cast.factor.factor
#' @export
#' @method vec_cast.character factor
vec_cast.character.factor <- function(x, to, ...) as.character(x)
#' @export
#' @method vec_cast.factor list
vec_cast.factor.list <- function(x, to, ..., x_arg = "", to_arg = "") {
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.factor default
vec_cast.factor.default <- function(x, to, ..., x_arg = "", to_arg = "") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
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
    # Can't use hash() currently because it hashes the string pointers
    # for performance, so the values in the test change each time
    substr(digest::digest(x), 1, length)
  }
}

levels_union <- function(x, y) {
  union(levels(x), levels(y))
}
