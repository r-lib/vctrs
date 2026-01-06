# Imported at load-time in `sf_env`
st_sfc = function(...) stop_sf()
st_crs = function(...) stop_sf()
st_precision = function(...) stop_sf()
st_as_sf = function(...) stop_sf()
st_is_empty = function(...) stop_sf()
stop_sf = function() abort("Internal error: Failed sf import.")

sf_deps = c(
  "st_sfc",
  "st_crs",
  "st_precision",
  "st_as_sf",
  "st_is_empty"
)
sf_env = env()


# sf namespace
local(envir = sf_env, {
  # Registered at load-time (same for all other methods)
  vec_proxy_sfc = function(x, ...) {
    # Strip attributes so they can't be accidentally used in `vec_restore()`.
    # Only `to` attributes should be considered in `vec_restore()`. Practically
    # this is important for the `classes` attribute. We want it to be fully
    # recomputed from the data in `x` because `st_sfc()` doesn't always restore
    # it correctly https://github.com/r-spatial/sf/issues/2567.
    sf_unstructure(x)
  }

  vec_restore_sfc = function(x, to, ...) {
    out = st_sfc(x, crs = st_crs(to), precision = st_precision(to))

    # If we lost our specific sf subtype due to a `vec_ptype()` + `vec_init()`
    # operation that looks like this:
    #
    # ```
    # x <- sf::st_sfc(sf::st_point(c(1, 2)))
    # ptype <- vec_ptype(x)
    # vec_init(ptype, 5)
    # ```
    #
    # Then we attempt to recover that sf subtype by reindexing with `[`,
    # replacing the indices corresponding to GEOMETRYCOLLECTION EMPTY with `NA`.
    # This ideally results in a homogeneous geometry type remaining in the
    # column, which sf will then use as the class type on the way out.
    if (inherits(out, "sfc_GEOMETRY")) {
      empty = st_is_empty(out)

      if (any(empty)) {
        # If they are empty, they must also be the abstract GEOMETRYCOLLECTION
        # to be considered for reindexing. Other empty types are meant to be there.
        empty[empty] <- vapply(
          unclass(out)[empty],
          FUN = function(x) inherits(x, "GEOMETRYCOLLECTION"),
          FUN.VALUE = logical(1)
        )
      }

      if (any(empty)) {
        loc = rep(TRUE, length(empty))
        loc[empty] = NA
        out = out[loc]
      }
    }

    out
  }

  vec_proxy_sf = function(x, ...) {
    # Strip attributes to ensure `vec_restore()`'s call to `st_as_sf()` uses the
    # data frame S3 method, and can't use any information from `x`'s original
    # `sf` state
    sf_unstructure(x)
  }

  vec_restore_sf = function(x, to, ...) {
    # Due to the way `vec_ptype()` works, `vec_df_restore()` will preemptively
    # restore the `to` attributes by straight up copying them over. We really
    # don't want that! `sf::st_as_sf()` needs to S3 dispatch to the data frame
    # method. If `to` attributes are preemptively restored (including the class)
    # then it will instead dispatch on the sf method, and will "reuse"
    # attributes from `x`, which is incorrect. It should only use `to`
    # attributes when restoring. See TODO in `vec_df_restore()`.
    x = sf_unstructure(x)

    sf_column_name = attr(to, "sf_column")
    crs = st_crs(to)
    prec = st_precision(to)

    st_as_sf(
      x,
      sf_column_name = sf_column_name,
      crs = crs,
      precision = prec,
      stringsAsFactors = FALSE
    )
  }

  # `vec_unstructure()` https://github.com/r-lib/vctrs/pull/2130
  sf_unstructure = function(x) {
    if (is.data.frame(x)) {
      x <- vctrs::new_data_frame(x, row.names = .row_names_info(x, 0L))
    } else if (!is.null(dim(x))) {
      attributes(x) <- list(dim = dim(x), dimnames = dimnames(x))
    } else {
      attributes(x) <- list(names = names(x))
    }
    x
  }

  sf_ptype2 = function(x, y, ...) {
    data = vctrs::df_ptype2(x, y, ...)

    # Take active geometry from left-hand side
    sf_column_name = attr(x, "sf_column")

    # CRS and precision must match
    crs = common_crs(x, y)
    prec = common_prec(x, y)

    st_as_sf(
      data,
      sf_column_name = sf_column_name,
      crs = crs,
      precision = prec,
      stringsAsFactors = FALSE
    )
  }

  vec_ptype2_sf_sf = function(x, y, ...) {
    sf_ptype2(x, y, ...)
  }

  vec_ptype2_sf_data.frame = function(x, y, ...) {
    vctrs::df_ptype2(x, y, ...)
  }
  vec_ptype2_data.frame_sf = function(x, y, ...) {
    vctrs::df_ptype2(x, y, ...)
  }

  vec_ptype2_sf_tbl_df = function(x, y, ...) {
    vctrs::tib_ptype2(x, y, ...)
  }
  vec_ptype2_tbl_df_sf = function(x, y, ...) {
    vctrs::tib_ptype2(x, y, ...)
  }

  sf_cast = function(x, to, ...) {
    data = vctrs::df_cast(x, to, ...)

    sf_column_name = attr(to, "sf_column")
    crs = st_crs(to)
    prec = st_precision(to)

    st_as_sf(
      data,
      sf_column_name = sf_column_name,
      crs = crs,
      precision = prec,
      stringsAsFactors = FALSE
    )
  }

  # Because `vec_ptype2.sf.sf()` returns a sf
  vec_cast_sf_sf = function(x, to, ...) {
    sf_cast(x, to, ...)
  }

  # Because `vec_ptype2.sf.data.frame()` returns a data frame
  vec_cast_data.frame_sf = function(x, to, ...) {
    vctrs::df_cast(x, to, ...)
  }
  # Opt out of `vec_default_cast()` support for data.frame -> sf.
  # Would never be called automatically, and likely not meaningful.
  vec_cast_sf_data.frame = function(x, to, ..., x_arg = "", to_arg = "") {
    stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }

  # Because `vec_ptype2.sf.tbl_df()` returns a tibble
  vec_cast_tbl_df_sf = function(x, to, ...) {
    vctrs::tib_cast(x, to, ...)
  }
  # Opt out of `vec_default_cast()` support for tibble -> sf.
  # Would never be called automatically, and likely not meaningful.
  vec_cast_sf_tbl_df = function(x, to, ..., x_arg = "", to_arg = "") {
    stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }

  vec_proxy_order_sfc <- function(x, ...) {
    # These are list columns, so they need to use the order-by-appearance proxy
    # that is defined by `vec_proxy_order.list()`
    x <- unstructure(x)
    vec_proxy_order(x)
  }

  # take conservative approach of requiring equal CRS and precision
  common_crs = function(x, y) {
    lhs = st_crs(x)
    rhs = st_crs(y)

    if (lhs != rhs) {
      stop("coordinate reference systems not equal: use st_transform() first?")
    }

    lhs
  }
  common_prec = function(x, y) {
    lhs = st_precision(x)
    rhs = st_precision(y)

    if (lhs != rhs) {
      stop("precisions not equal")
    }

    lhs
  }
}) # local(envir = sf_env)

env_bind(ns_env("vctrs"), !!!as.list(sf_env))
