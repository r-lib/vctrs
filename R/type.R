# The type of a vector is a compact string representation
vec_type <- function(x) UseMethod("vec_type")

# The "dimension" type ignores the length (the first dimension)
dim_type <- function(x) {
  if (vec_dims(x) == 1) {
    "[]"
  } else {
    paste0("[,", paste(vec_dim(x)[-1], collapse = ","), "]")
  }
}

vec_type.default <- function(x) {
  if (is.object(x)) {
    class(x)[[1]]
  } else {
    paste0(typeof(x), dim_type(x))
  }
}

vec_type.Date <- function(x) {
  paste0("date", dim_type(x))
}
vec_type.POSIXt <- function(x) {
  paste0("datetime", dim_type(x))
}

# Levels are parameter of the type, because it does not make sense to
# compare the values of the underlying vector if the levels are different.
vec_type.factor <- function(x) {
  params <- paste0("<", paste0(levels(x), collapse = ","), ">")
  paste0("factor",dim_type(x), params)
}

vec_type.data.frame <- function(x) {
  # Needs to handle recursion with indenting
  types <- map_chr(x, vec_type)

  paste0(
    "data.frame<\n",
    paste0("  $", format(names(x)), " ", types, collapse = "\n"),
    "\n>"
  )
}

