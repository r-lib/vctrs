
# Imported at load-time in `sf_env`
st_crs = function(...) stop_sf()
st_precision = function(...) stop_sf()
st_as_sf = function(...) stop_sf()
stop_sf = function() abort("Internal error: Failed sf import.")

sf_deps = c(
	"st_crs",
	"st_precision",
	"st_as_sf"
)
sf_env = env()


# sf namespace
local(envir = sf_env, {

# Registered at load-time (same for all other methods)
vec_proxy.sf = function(x, ...) {
	x
}

vec_restore.sf = function(x, to, ...) {
	sfc_name = attr(to, "sf_column")
	crs = st_crs(to)
	prec = st_precision(to)

	st_as_sf(
		x,
		sf_column_name = sfc_name,
		crs = crs,
		precision = prec,
		stringsAsFactors = FALSE
	)
}

sf_ptype2 = function(x, y, ...) {
	data = vctrs::df_ptype2(x, y, ...)

	# Workaround for `c()` fallback sentinels. Must be fixed before
	# moving the methods downstream.
	opts <- match_fallback_opts(...)
	if (identical(opts$s3_fallback, S3_FALLBACK_true)) {
		return(data)
	}

	x_sf <- inherits(x, "sf")
	y_sf <- inherits(y, "sf")

	if (x_sf && y_sf) {
		# Take active geometry from left-hand side
		sfc_name = attr(x, "sf_column")

		# CRS and precision must match
		crs = common_crs(x, y)
		prec = common_prec(x, y)
	} else if (x_sf) {
		sfc_name = attr(x, "sf_column")
		crs = st_crs(x)
		prec = st_precision(x)
	} else if (y_sf) {
		sfc_name = attr(y, "sf_column")
		crs = st_crs(y)
		prec = st_precision(y)
	} else {
		stop("Internal error: Expected at least one `sf` input.")
	}

	st_as_sf(
		data,
		sf_column_name = sfc_name,
		crs = crs,
		precision = prec,
		stringsAsFactors = FALSE
	)
}

vec_ptype2.sf.sf = function(x, y, ...) {
	sf_ptype2(x, y, ...)
}
vec_ptype2.sf.data.frame = function(x, y, ...) {
	sf_ptype2(x, y, ...)
}
vec_ptype2.data.frame.sf = function(x, y, ...) {
	sf_ptype2(x, y, ...)
}

# Maybe we should not have these methods, but they are currently
# required to avoid the base-df fallback
vec_ptype2.sf.tbl_df = function(x, y, ...) {
	new_data_frame(sf_ptype2(x, y, ...))
}
vec_ptype2.tbl_df.sf = function(x, y, ...) {
	new_data_frame(sf_ptype2(x, y, ...))
}

sf_cast = function(x, to, ...) {
	data = vctrs::df_cast(x, to, ...)

	# Workaround for `c()` fallback sentinels. Must be fixed before
	# moving the methods downstream.
	opts <- match_fallback_opts(...)
	if (identical(opts$s3_fallback, S3_FALLBACK_true)) {
		return(data)
	}

	sfc_name = attr(to, "sf_column")
	crs = st_crs(to)
	prec = st_precision(to)

	st_as_sf(
		data,
		sf_column_name = sfc_name,
		crs = crs,
		precision = prec,
		stringsAsFactors = FALSE
	)
}

vec_cast.sf.sf = function(x, to, ...) {
	sf_cast(x, to, ...)
}
vec_cast.sf.data.frame = function(x, to, ...) {
	sf_cast(x, to, ...)
}
vec_cast.data.frame.sf = function(x, to, ...) {
	df_cast(x, to, ...)
}

vec_proxy_order.sfc <- function(x, ...) {
  # These are list columns, so they need to use the order-by-appearance proxy
  # that is defined by `vec_proxy_order.list()`
  x <- unstructure(x)
  vec_proxy_order(x)
}

# take conservative approach of requiring equal CRS and precision
common_crs = function(x, y) {
	lhs = st_crs(x)
	rhs = st_crs(y)

	if (lhs != rhs)
		stop("coordinate reference systems not equal: use st_transform() first?")

	lhs
}
common_prec = function(x, y) {
	lhs = st_precision(x)
	rhs = st_precision(y)

	if (lhs != rhs)
		stop("precisions not equal")

	lhs
}

}) # local(envir = sf_env)

env_bind(ns_env("vctrs"), !!!as.list(sf_env))

# Local Variables:
# indent-tabs-mode: t
# ess-indent-offset: 4
# tab-width: 4
# End:
