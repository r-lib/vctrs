#' Recycle vector types to common number of observations
#'
#' @param ... Vectors to recycle.
#' @param .nobs Number of observations
#' @return A list of vectors each with equal [vec_obs()], or an error
#'   stating that recycling was not possible.
#' @export
#' @examples
#' # Inputs with 1 observation are recycled
#' vec_recycle(1:5, 5)
#' \dontrun{
#' vec_recycle(1:5, 1:2)
#' }
#'
#' # Inputs with 0 observations
#' vec_recycle(1:5, integer())
#'
#' # Data frames and matrices are recycled along their rows
#' vec_recycle(data.frame(x = 1), 1:5)
#' vec_recycle(array(1:2, c(1, 2)), 1:5)
#' vec_recycle(array(1:3, c(1, 3, 1)), 1:5)
vec_recycle <- function(..., .nobs = NULL) {
  args <- list2(...)
  if (length(args) == 0)
    return(list())
  nobs <- vec_shape(!!!args, .nobs = .nobs)
  map(args, vec_reshape, nobs)
}

vec_shape <- function(..., .nobs = NULL) {
  if (!is.null(.nobs)) {
    return(.nobs)
  }

  args <- compact(list2(...))
  nobs <- map_int(args, vec_obs)
  reduce(nobs, recycle_length)
}

vec_reshape <- function(x, n_to) {
  if (is.null(x) || is.null(n_to))
    return(x)

  n_x <- vec_obs(x)

  if (n_x == n_to) {
    x
  } else if (n_to == 0L) {
    vec_slice(x, 0L)
  } else if (n_x == 1L) {
    vec_rep(x, n_to)
  } else {
    stop("Incompatible lengths: ", n_x, ", ", n_to, call. = FALSE)
  }
}
