
# `numeric_version` from base ----------------------------------------

#' @export
vec_proxy.numeric_version <- function(x, ...) {
  x
}

#' @export
vec_proxy_equal.numeric_version <- function(x, ...) {
  proxy_equal_numeric_version(x)
}

# To generate data agnostic proxies of `<numeric_version>`, we enforce a
# restriction that each version can have at most 8 components. This allows us
# to `vec_compare()` them without needing a "joint" comparison proxy, unlike
# what `.encode_numeric_version()` returns.
proxy_equal_numeric_version <- function(x, error_call = caller_env()) {
  N_COMPONENTS <- 8L

  x <- unclass(x)

  size <- length(x)
  sizes <- lengths(x)

  if (length(sizes) != 0L) {
    max <- max(sizes)
  } else {
    max <- N_COMPONENTS
  }

  if (max > N_COMPONENTS) {
    cli::cli_abort(
      "`x` can't contain more than {N_COMPONENTS} version components.",
      call = error_call
    )
  }

  if (any(sizes != max)) {
    # Pad with zeros where needed to be able to transpose.
    # This is somewhat slow if required.
    pad_sizes <- max - sizes
    pad_needed <- which(pad_sizes != 0L)

    x[pad_needed] <- map2(
      x[pad_needed],
      pad_sizes[pad_needed],
      function(elt, pad_size) {
        c(elt, vec_rep(0L, times = pad_size))
      }
    )
  }

  # Transpose with combination of `vec_interleave()` and `vec_chop()`
  x <- vec_interleave(!!!x, .ptype = integer())

  # TODO: `vec_chop(sizes = vec_rep(size, times = max))`
  index <- seq_len(size)
  indices <- vector("list", length = max)

  for (i in seq_len(max)) {
    indices[[i]] <- index
    index <- index + size
  }

  out <- vec_chop(x, indices = indices)

  n_zeros <- N_COMPONENTS - max

  if (n_zeros != 0L) {
    # Pad columns of zeros out to `N_COMPONENTS` columns
    zero <- list(vec_rep(0L, times = size))
    out <- c(out, vec_rep(zero, times = n_zeros))
  }

  # Use a data frame as the proxy
  names(out) <- paste0("...", seq_len(N_COMPONENTS))
  out <- new_data_frame(out, n = size)

  # A `<numeric_version>` internally stored as `integer()` is considered the
  # `NA` value. We patch that in at the very end if needed. It is hard to create
  # so should be very uncommon.
  missing <- sizes == 0L

  if (any(missing)) {
    na <- vec_init(out)
    out <- vec_assign(out, missing, na)
  }

  out
}

# `omit` from base ---------------------------------------------------

#' @export
vec_proxy.omit <- function(x, ...) {
  x
}
#' @export
vec_restore.omit <- function(x, ...) {
  structure(x, class = "omit")
}

#' @export
vec_ptype2.omit.omit <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.integer.omit <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.omit.integer <- function(x, y, ...) {
  y
}
#' @export
vec_ptype2.double.omit <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.omit.double <- function(x, y, ...) {
  y
}

#' @export
vec_cast.omit.omit <- function(x, to, ...) {
  x
}
#' @export
vec_cast.integer.omit <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.omit.integer <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.double.omit <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.omit.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}


# `exclude` from base ------------------------------------------------

#' @export
vec_proxy.exclude <- function(x, ...) {
  x
}
#' @export
vec_restore.exclude <- function(x, ...) {
  structure(x, class = "exclude")
}

#' @export
vec_ptype2.exclude.exclude <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.integer.exclude <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.exclude.integer <- function(x, y, ...) {
  y
}
#' @export
vec_ptype2.double.exclude <- function(x, y, ...) {
  x
}
#' @export
vec_ptype2.exclude.double <- function(x, y, ...) {
  y
}

#' @export
vec_cast.exclude.exclude <- function(x, to, ...) {
  x
}
#' @export
vec_cast.integer.exclude <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.exclude.integer <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.double.exclude <- function(x, to, ...) {
  vec_cast(vec_data(x), to, ...)
}
#' @export
vec_cast.exclude.double <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}


# `data.table` -------------------------------------------------------

delayedAssign("as.data.table", {
  if (is_installed("data.table")) {
    env_get(ns_env("data.table"), "as.data.table")
  } else {
    function(...) abort("`data.table` must be installed.")
  }
})

dt_ptype2 <- function(x, y, ...) {
  as.data.table(df_ptype2(x, y, ...))
}
dt_cast <- function(x, to, ...) {
  as.data.table(df_cast(x, to, ...))
}

#' @export
vec_ptype2.data.table.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.table.data.frame <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}
#' @export
vec_ptype2.data.frame.data.table <- function(x, y, ...) {
  dt_ptype2(x, y, ...)
}

#' @export
vec_cast.data.table.data.table <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.data.table.data.frame <- function(x, to, ...) {
  dt_cast(x, to, ...)
}
#' @export
vec_cast.data.frame.data.table <- function(x, to, ...) {
  df_cast(x, to, ...)
}

#' @export
vec_ptype_abbr.data.table <- function(x, ...) {
  "dt"
}
