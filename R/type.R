# The type of a vector is a compact string representation
# Compared to type_sum it is not designed to fit in a column label
# So can be quite a lot longer

vec_type <- function(x) UseMethod("vec_type")

#' @export
vec_type.default <- function(x) {
  stopifnot(is_vector(x))

  vt(typeof(x), dim_type(x))
}

#' @export
vec_type.Date <- function(x) {
  vt("date")
}

#' @export
vec_type.POSIXt <- function(x) {
  vt("datetime")
}

# Levels are parameter of the type, because it does not make sense to
# compare the values of the underlying vector if the levels are different.
# Levels are potentially long, so we just display a hash of the levels.
#' @export
vec_type.factor <- function(x) {
  params <- paste0("<", hash(levels(x)), ">")
  vt("factor", dim_type(x), params)
}

#' @export
vec_type.data.frame <- function(x) {
  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_type)

  vt(
    "data.frame<\n",
    paste0("  $", format(names(x)), " ", types, collapse = "\n"),
    "\n>"
  )
}

# The "length" is not included in the type specification since it's often
# not important (and the recycling rules means matching is non trivial)
dim_type <- function(x) {
  if (vec_dims(x) == 1) {
    ""
  } else {
    paste0("[,", paste(vec_dim(x)[-1], collapse = ","), "]")
  }
}


vt <- function(...) {
  structure(paste0(...), class = "vec_type")
}

#' @export
print.vec_type <- function(x, ...) {
  cat(x, "\n", sep = "")
  invisible(x)
}
