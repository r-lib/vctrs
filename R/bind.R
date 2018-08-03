vec_rbind <- function(..., .type = NULL) {
  args <- list2(...)
  tbls <- map(args, as_tibble2)
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

# as_tibble2 --------------------------------------------------------------

as_tibble2 <- function(x) UseMethod("as_tibble2")

# important that this doesn't convert data frames to tibbles
#' @export
as_tibble2.data.frame <- function(x) x

#' @export
as_tibble2.NULL <- function(x) x

#' @export
as_tibble2.default <- function(x) {
  if (vec_dims(x) == 1L) {
    x <- as.list(x)
    x <- tibble::set_tidy_names(x)
  }

  # TODO: eliminate this
  tibble::as_tibble(x)
}
