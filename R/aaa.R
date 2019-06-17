
# nocov start

# Useful for micro-optimising default arguments requiring evaluation,
# such as `param = c("foo", "bar")`. Buys about 0.6us on my desktop.
fn_inline_formals <- function(fn, names) {
  stopifnot(typeof(fn) == "closure")

  fmls <- formals(fn)
  fmls[names] <- lapply(fmls[names], eval)

  formals(fn) <- fmls
  fn
}

# nocov end
