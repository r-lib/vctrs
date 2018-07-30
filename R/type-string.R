vec_type_string <- function(x) UseMethod("vec_type_string")

#' @export
vec_type_string.NULL <- function(x) {
  "NULL"
}

#' @export
vec_type_string.default <- function(x) {
  paste0(typeof(x), dim_type(x))
}

#' @export
vec_type_string.Date <- function(x) {
  "date"
}

#' @export
vec_type_string.POSIXt <- function(x) {
  "datetime"
}

# Levels are parameter of the type, because it does not make sense to
# compare the values of the underlying vector if the levels are different.
# Levels are potentially long, so we just display a hash of the levels.
#' @export
vec_type_string.factor <- function(x) {
  params <- paste0("<", hash(levels(x)), ">")
  paste0("factor", dim_type(x), params)
}

#' @export
vec_type_string.difftime <- function(x) {
  params <- paste0("<", attr(x, "units"), ">")
  paste0("difftime", dim_type(x), params)
}


#' @export
vec_type_string.data.frame <- function(x) {
  if (length(x) == 0) {
    return("data.frame<>")
  } else if (length(x) == 1) {
    return(paste0("data.frame<", names(x), ":", vec_type_string(x[[1]]), ">"))
  }

  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_type_string)

  names <- paste0(" ", format(names(x)))
  types <- indent(types, nchar(names) + 1)

  paste0(
    "data.frame<\n",
      paste0(names, ": ", types, collapse = "\n"),
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
