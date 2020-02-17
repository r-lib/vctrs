# Type2 -------------------------------------------------------------------

# Left generics -----------------------------------------------------------

#' @rdname vec_ptype2
#' @export vec_ptype2.logical
#' @method vec_ptype2 logical
#' @export
vec_ptype2.logical <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.logical", y)
  }
}
#' @rdname vec_ptype2
#' @export vec_ptype2.integer
#' @method vec_ptype2 integer
#' @export
vec_ptype2.integer <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.integer", y)
  }
}
#' @rdname vec_ptype2
#' @export vec_ptype2.double
#' @method vec_ptype2 double
#' @export
vec_ptype2.double <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.double", y)
  }
}
#' @rdname vec_ptype2
#' @export vec_ptype2.complex
#' @method vec_ptype2 complex
#' @export
vec_ptype2.complex <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.complex", y)
  }
}
#' @rdname vec_ptype2
#' @export vec_ptype2.character
#' @method vec_ptype2 character
#' @export
vec_ptype2.character <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.character", y)
  }
}
#' @rdname vec_ptype2
#' @export vec_ptype2.raw
#' @method vec_ptype2 raw
#' @export
vec_ptype2.raw <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.raw", y)
  }
}
#' @rdname vec_ptype2
#' @export vec_ptype2.list
#' @method vec_ptype2 list
#' @export
vec_ptype2.list <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(x)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    UseMethod("vec_ptype2.list", y)
  }
}


# Numeric-ish

#' @method vec_ptype2.logical logical
#' @export
vec_ptype2.logical.logical <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else if (is_unspecified(x) && is_unspecified(y)) {
    # Special case `vec_ptype2(NA, NA)` to ensure that
    # `unspecified()` is returned
    unspecified()
  } else {
    shape_match(logical(), x, y)
  }
}

#' @export
#' @method vec_ptype2.integer integer
vec_ptype2.integer.integer <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(integer(), x, y)
  }
}
#' @export
#' @method vec_ptype2.logical integer
vec_ptype2.logical.integer <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(integer(), x, y)
  }
}
#' @export
#' @method vec_ptype2.integer logical
vec_ptype2.integer.logical <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(integer(), x, y)
  }
}

#' @export
#' @method vec_ptype2.double double
vec_ptype2.double.double <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(double(), x, y)
  }
}
#' @export
#' @method vec_ptype2.logical double
vec_ptype2.logical.double <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(double(), x, y)
  }
}
#' @export
#' @method vec_ptype2.double logical
vec_ptype2.double.logical <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(double(), x, y)
  }
}
#' @export
#' @method vec_ptype2.integer double
vec_ptype2.integer.double <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(double(), x, y)
  }
}
#' @export
#' @method vec_ptype2.double integer
vec_ptype2.double.integer <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(double(), x, y)
  }
}

#' @export
#' @method vec_ptype2.complex complex
vec_ptype2.complex.complex <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(complex(), x, y)
  }
}
#' @export
#' @method vec_ptype2.integer complex
vec_ptype2.integer.complex <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(complex(), x, y)
  }
}
#' @export
#' @method vec_ptype2.complex integer
vec_ptype2.complex.integer <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(complex(), x, y)
  }
}
#' @export
#' @method vec_ptype2.double complex
vec_ptype2.double.complex <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(complex(), x, y)
  }
}
#' @export
#' @method vec_ptype2.complex double
vec_ptype2.complex.double <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(complex(), x, y)
  }
}



# Character

#' @method vec_ptype2.character character
#' @export
vec_ptype2.character.character <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(character(), x, y)
  }
}


# Raw

#' @export
#' @method vec_ptype2.raw raw
vec_ptype2.raw.raw <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(raw(), x, y)
  }
}


# Lists

#' @method vec_ptype2.list list
#' @export
vec_ptype2.list.list <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is.object(y)) {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  } else {
    shape_match(list(), x, y)
  }
}


# Default

#' @method vec_ptype2.logical default
#' @export
vec_ptype2.logical.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  if (is_unspecified(x)) {
    vec_ptype(y)
  } else {
    vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
  }
}
#' @method vec_ptype2.integer default
#' @export
vec_ptype2.integer.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.double default
#' @export
vec_ptype2.double.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.complex default
#' @export
vec_ptype2.complex.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.character default
#' @export
vec_ptype2.character.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.raw default
#' @export
vec_ptype2.raw.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
#' @method vec_ptype2.list default
#' @export
vec_ptype2.list.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
}


# Cast --------------------------------------------------------------------

# These methods for base types are handled at the C level unless
# inputs have shape or have lossy casts

#' @export
#' @rdname vec_cast
#' @export vec_cast.logical
#' @method vec_cast logical
vec_cast.logical <- function(x, to, ...) {
  UseMethod("vec_cast.logical")
}
#' @export
#' @method vec_cast.logical logical
vec_cast.logical.logical <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.logical integer
vec_cast.logical.integer <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  out <- vec_coerce_bare(x, "logical")
  out <- shape_broadcast(out, to)
  lossy <- !x %in% c(0L, 1L, NA_integer_)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.logical double
vec_cast.logical.double <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  out <- vec_coerce_bare(x, "logical")
  out <- shape_broadcast(out, to)
  lossy <- !x %in% c(0, 1, NA_real_)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.logical character
vec_cast.logical.character <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  out <- vec_coerce_bare(x, "logical")
  out <- shape_broadcast(out, to)
  lossy <- !x %in% c("T", "F", "TRUE", "FALSE", "true", "false", NA_character_)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.logical list
vec_cast.logical.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.logical default
vec_cast.logical.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.integer
#' @method vec_cast integer
vec_cast.integer <- function(x, to, ...) {
  UseMethod("vec_cast.integer")
}
#' @export
#' @method vec_cast.integer logical
vec_cast.integer.logical <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  x <- vec_coerce_bare(x, "integer")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.integer integer
vec_cast.integer.integer <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.integer double
vec_cast.integer.double <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  out <- suppressWarnings(vec_coerce_bare(x, "integer"))
  x_na <- is.na(x)
  lossy <- (out != x & !x_na) | xor(x_na, is.na(out))
  out <- shape_broadcast(out, to)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.integer character
vec_cast.integer.character <- vec_cast.integer.double
#' @export
#' @method vec_cast.integer list
vec_cast.integer.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.integer default
vec_cast.integer.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.double
#' @method vec_cast double
vec_cast.double <- function(x, to, ...) {
  UseMethod("vec_cast.double")
}
#' @export
#' @method vec_cast.double logical
vec_cast.double.logical <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  x <- vec_coerce_bare(x, "double")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.double integer
vec_cast.double.integer <- vec_cast.double.logical
#' @export
#' @method vec_cast.double character
vec_cast.double.character <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  out <- suppressWarnings(vec_coerce_bare(x, "double"))
  x_na <- is.na(x)
  lossy <- (out != x & !x_na) | xor(x_na, is.na(out))
  out <- shape_broadcast(out, to)
  maybe_lossy_cast(out, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.double double
vec_cast.double.double <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.double list
vec_cast.double.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.double default
vec_cast.double.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.complex
#' @method vec_cast complex
vec_cast.complex <- function(x, to, ...) {
  UseMethod("vec_cast.complex")
}
#' @export
#' @method vec_cast.complex logical
vec_cast.complex.logical <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  x <- vec_coerce_bare(x, "complex")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.complex integer
vec_cast.complex.integer <- vec_cast.complex.logical
#' @export
#' @method vec_cast.complex double
vec_cast.complex.double <- vec_cast.complex.logical
#' @export
#' @method vec_cast.complex complex
vec_cast.complex.complex <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.complex list
vec_cast.complex.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.complex default
vec_cast.complex.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.raw
#' @method vec_cast raw
vec_cast.raw <- function(x, to, ...) {
  UseMethod("vec_cast.raw")
}
#' @export
#' @method vec_cast.raw raw
vec_cast.raw.raw <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.raw list
vec_cast.raw.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.raw default
vec_cast.raw.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @export
#' @rdname vec_cast
#' @export vec_cast.character
#' @method vec_cast character
vec_cast.character <- function(x, to, ...) {
  UseMethod("vec_cast.character")
}
#' @export
#' @method vec_cast.character logical
vec_cast.character.logical <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  x <- vec_coerce_bare(x, "character")
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.character integer
vec_cast.character.integer <- vec_cast.character.logical
#' @export
#' @method vec_cast.character double
vec_cast.character.double <- vec_cast.character.logical
#' @export
#' @method vec_cast.character character
vec_cast.character.character <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.character difftime
vec_cast.character.difftime <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (!inherits_only(x, "difftime")) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  x <- paste(x, units(x))
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.character list
vec_cast.character.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  vec_list_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
#' @method vec_cast.character default
vec_cast.character.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

#' @rdname vec_cast
#' @export vec_cast.list
#' @method vec_cast list
#' @export
vec_cast.list <- function(x, to, ...) {
  UseMethod("vec_cast.list")
}
#' @export
#' @method vec_cast.list list
vec_cast.list.list <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }
  shape_broadcast(x, to)
}
#' @export
#' @method vec_cast.list default
vec_cast.list.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  if (is.object(x)) {
    return(vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg))
  }

  vec_cast_list_default(x, to, x_arg = x_arg, to_arg = to_arg)
}

vec_cast_list_default <- function(x, to, ..., x_arg = x_arg, to_arg = to_arg) {
  out <- lapply(seq_along(x), function(i) x[[i]])

  vec_slice(out, vec_equal_na(x)) <- list(NULL)

  if (!is.object(to)) {
    out <- shape_broadcast(out, to)
  }

  out
}

#' @export
#' @method vec_cast.list data.frame
vec_cast.list.data.frame <- function(x, to, ...) {
  # FIXME - Replace with the `vec_chop()`
  # equivalent for `vec_get()`
  row.names(x) <- NULL
  out <- vec_chop(x)

  vec_slice(out, vec_equal_na(x)) <- list(NULL)

  if (!is.object(to)) {
    out <- shape_broadcast(out, to)
  }

  out
}

# compare ------------------------------------------------------------

#' @export
vec_proxy_compare.raw <- function(x, ...) {
  # because:
  # order(as.raw(1:3))
  # #> Error in order(as.raw(1:3)): unimplemented type 'raw' in 'orderVector1'
  as.integer(x)
}


# Helpers -----------------------------------------------------------------

lossy_floor <- function(x, to, x_arg = "x", to_arg = "to") {
  x_floor <- floor(x)
  lossy <- x != x_floor
  maybe_lossy_cast(x_floor, x, to, lossy, x_arg = x_arg, to_arg = to_arg)
}

shape_match <- function(type, x, y) {
  if (!is.object(x) && !is.object(y)) {
    shape <- shape_common(x, y)
    new_shape(type, shape)
  } else {
    type
  }
}
