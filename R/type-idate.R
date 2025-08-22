# Proxy method for <IDate>
#
# `vec_proxy.Date()` coerces integer storage dates to double. If we don't
# intercept that, an IDate will also be converted to double storage, but that
# results in a corrupt IDate. Since we provide some methods for <data.table>, we
# also provide this one.
#
# Notably we don't provide `vec_ptype2.Date.IDate` methods. We don't think it is
# our place to provide methods for that, especially since we'd likely prefer
# <Date> as the more general common type, but `c.IDate` in data.table prefers
# <IDate>, so we'd probably just add conflicting behavior.
vec_proxy_IDate <- function(x, ...) {
  if (typeof(x) != "integer") {
    type <- typeof(x)
    cli::cli_abort(
      "Corrupt <IDate>. Expected integer storage, not {type} storage."
    )
  }

  x
}

# Restore method for <IDate>
#
# Follow `vec_restore.Date` and pass through to `vec_restore.default` which
# uses standard restore behavior
vec_restore_IDate <- function(x, to, ...) {
  NextMethod()
}
