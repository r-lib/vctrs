vec_rbind <- function(..., .type = NULL) {
  args <- list2(...)
  tbls <- map(args, as_tibble_row)
  type <- find_type(tbls, .type = .type)

  if (is.null(type))
    return(data.frame())

  ns <- map_int(tbls, vec_length)
  out <- vec_rep(type, sum(ns))
  rownames(out) <- NULL

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    out[pos:(pos + n - 1), ] <- vec_cast(tbls[[i]], to = type)
    pos <- pos + n
  }

  out
}


vec_cbind <- function(..., .type = NULL) {
  args <- list2(...)

  # needs name if no outer name, and is vector/matrix without names
  no_outer <- names2(args) == ""
  no_inner <- map_lgl(args, function(x) vec_dims(x) == 1 || is.null(colnames(x)))
  name_fix <- no_outer & no_inner
  names(args) <- ifelse(name_fix, paste0("X", seq_along(args)), names2(args))

  tbls <- map2(args, names2(args), as_tibble_col)
  names(tbls) <- NULL

  # recycle to same length
  nrows <- map_int(tbls, NROW)
  n <- Reduce(recycle_length, nrows)
  tbls <- map(tbls, recycle, n = n)

  ns <- map_int(tbls, length)
  out <- vec_rep(list(), sum(ns))
  names <- vec_rep(character(), sum(ns))

  pos <- 1
  for (i in seq_along(ns)) {
    n <- ns[[i]]
    if (n == 0L)
      next

    out[pos:(pos + n - 1)] <- tbls[[i]]
    if (!name_fix[[n]])
      names[pos:(pos + n - 1)] <- names(tbls[[i]])
    pos <- pos + n
  }

  names(out) <- tibble::tidy_names(names)
  as.data.frame(tibble::new_tibble(out))
}


recycle <- function(x, n) {
  if (is.null(x) || nrow(x) == n)
    return(x)

  vec_rep(x, n)
}

# as_tibble --------------------------------------------------------------

as_tibble_row <- function(x) UseMethod("as_tibble_row")

# important that this doesn't convert data frames to tibbles
#' @export
as_tibble_row.data.frame <- function(x) x

#' @export
as_tibble_row.NULL <- function(x) x

#' @export
as_tibble_row.default <- function(x) {
  if (vec_dims(x) == 1L) {
    x <- as.list(x)
    x <- tibble::set_tidy_names(x)
  }

  # TODO: eliminate this
  tibble::as_tibble(x)
}


as_tibble_col <- function(x, outer_name) UseMethod("as_tibble_col")

#' @export
as_tibble_col.data.frame <- function(x, outer_name = NULL) {
  names(x) <- outer_names(x, outer_name)
  x
}

#' @export
as_tibble_col.NULL <- function(x, outer_name = NULL) x

#' @export
as_tibble_col.default <- function(x, outer_name = NULL) {
  if (vec_dims(x) == 1L) {
    tibble::as_tibble(setNames(list(x), outer_name), validate = FALSE)
  } else {
    colnames(x) <- outer_names(x, outer_name)
    # TODO: eliminate this
    tibble::as_tibble(x)
  }
}
