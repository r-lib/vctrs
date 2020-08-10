#' Flatten nested lists
#'
#' @description
#'
#' `list_flatten()` flattens one level of nested lists:
#'
#' ```{r}
#' x <- list(1, list(2), 3)
#' str(x)
#' str(list_flatten(x))
#' ```
#'
#' If the input does not contain any nested list, it is returned as is:
#'
#' ```{r}
#' list_flatten(list(1, 2, 3))
#' ```
#'
#' Only one level of nested list is flattened:
#'
#' ```{r}
#' x <- list(1, list(2), list(list(3)))
#' str(x)
#' str(list_flatten(x))
#' ```
#'
#' Note that data frames are not treated as lists. See
#' [vec_is_list()].
#'
#' @param x A list.
#' @inheritParams ellipsis::dots_empty
#' @inheritParams vec_unchop
#'
#' @noRd
list_flatten <- function(x, ..., ptype = x, name_spec = NULL) {
  ellipsis::check_dots_empty()
  list_assert(x)

  # Wrap any non-list elements
  out <- map_if(x, negate(vec_is_list), list)

  # Concatenate the elements (now all lists) in a single list
  vec_unchop(out, ptype = ptype, name_spec = name_spec)
}

list_assert <- function(x, arg = as_label(substitute(x))) {
  if (!vec_is_list(x)) {
    abort(glue::glue("`{arg}` must be a list."))
  }
}
