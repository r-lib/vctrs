
# Rational record class from the S3 vector vignette

new_rational <- function(n = integer(), d = integer()) {
  vec_assert(n, ptype = integer())
  vec_assert(d, ptype = integer())
  new_rcrd(list(n = n, d = d), class = "vctrs_rational")
}

rational <- function(n, d) {
  c(n, d) %<-% vec_cast_common(n, d, .to = integer())
  c(n, d) %<-% vec_recycle_common(n, d)
  new_rational(n, d)
}

format.vctrs_rational <- function(x, ...) {
  n <- field(x, "n")
  d <- field(x, "d")
  out <- paste0(n, "/", d)
  out[is.na(n) | is.na(d)] <- NA
  out
}

vec_proxy_equal.vctrs_rational <- function(x) {
  n <- field(x, "n")
  d <- field(x, "d")
  gcd <- gcd(n, d)
  data.frame(n = n / gcd, d = d / gcd)
}
gcd <- function(x, y) {
  r <- x %% y
  ifelse(r, gcd(y, r), y)
}

vec_proxy_compare.vctrs_rational <- function(x) {
  field(x, "n") / field(x, "d")
}

rational_methods <- list(
  vec_ptype_abbr.vctrs_rational = function(x, ...) "rtnl",
  vec_ptype_full.vctrs_rational = function(x, ...) "rational",

  vec_ptype2.vctrs_rational = function(x, y, ...) UseMethod("vec_ptype2.vctrs_rational", y),
  vec_ptype2.vctrs_rational.default = function(x, y, ...) vec_default_ptype2(x, y, ...),
  vec_ptype2.vctrs_rational.vctrs_rational = function(x, y, ...) new_rational(),
  vec_ptype2.vctrs_rational.integer = function(x, y, ...) new_rational(),
  vec_ptype2.integer.vctrs_rational = function(x, y, ...) new_rational(),

  vec_cast.vctrs_rational = function(x, to, ...) UseMethod("vec_cast.vctrs_rational"),
  vec_cast.vctrs_rational.default = function(x, to, ...) vec_default_cast(x, to),
  vec_cast.vctrs_rational.vctrs_rational = function(x, to, ...) x,
  vec_cast.double.vctrs_rational = function(x, to, ...) field(x, "n") / field(x, "d"),
  vec_cast.vctrs_rational.integer = function(x, to, ...) rational(x, 1),

  vec_proxy_equal.vctrs_rational = vec_proxy_equal.vctrs_rational,
  vec_proxy_compare.vctrs_rational = vec_proxy_compare.vctrs_rational
)

scoped_rational_class <- function(frame = caller_env()) {
  scoped_global_bindings(.frame = frame, !!!rational_methods)
}

env_bind(global_env(), !!!rational_methods)
