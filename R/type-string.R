#' Vector type as a string
#'
#' Provide a method for this generic to control how your vector type is
#' printed. The default method uses the first element of the class vector.
#' Override this method if your class has parameters that should be prominently
#' displayed.
#'
#' @param x A vector.
#' @keywords internal
#' @return A string.
#' @export
#' @examples
#' cat(vec_type_string(1:10))
#' cat(vec_type_string(iris))
vec_type_string <- function(x) {
  UseMethod("vec_type_string")
}

#' @export
vec_type_string.NULL <- function(x) {
  "NULL"
}

#' @export
vec_type_string.default <- function(x) {
  if (is.object(x)) {
    class(x)[[1]]
  } else {
    paste0(typeof(x), dim_type(x))
  }
}

#' @export
vec_type_string.Date <- function(x) {
  "date"
}

#' @export
vec_type_string.POSIXt <- function(x) {
  paste0("datetime<", attr(x, "tzone") %||% "local", ">")
}

# Strict thinking:
# Levels are parameter of the type, because it does not make sense to
# compare the values of the underlying vector if the levels are different.
# Levels are potentially long, so we just display a hash of the levels.
# Relaxed thinking:
# Pragmatically, while the levels are a parameter of the type, the pay off
# for explicitly managing is not worthwhile.
#' @export
vec_type_string.factor <- function(x) {
  paste0(class(x)[[1]], "<", hash(levels(x)), ">")
}

#' @export
vec_type_string.difftime <- function(x) {
  paste0("difftime<", attr(x, "units"), ">")
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
  needs_indent <- grepl("\n", types)
  types[needs_indent] <- map(types[needs_indent], function(x) indent(paste0("\n", x), 4))

  names <- paste0("  ", format(names(x)))

  paste0(
    "data.frame<\n",
      paste0(names, ": ", types, collapse = "\n"),
    "\n>"
  )
}

# In the type specification of bare vectors, a zero means free specification
dim_type <- function(x) {
  if (vec_dims(x) == 1) {
    ""
  } else {
    dim <- vec_dim(x)
    paste0("[", paste(ifelse(dim == 0L, "", dim), collapse = ","), "]")
  }
}
