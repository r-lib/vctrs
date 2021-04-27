on_load <- function(expr, env = parent.frame()) {
  ns <- topenv(env)
  expr <- substitute(expr)
  callback <- function() eval_bare(expr, env)
  ns$.__rlang_hook__. <- c(ns$.__rlang_hook__., list(callback))
}

run_on_load <- function(env = caller_env()) {
  ns <- topenv(env)

  hook <- ns$.__rlang_hook__.
  env_unbind(ns, ".__rlang_hook__.")

  for (callback in hook) {
    callback()
  }

  ns$.__rlang_hook__. <- NULL
}


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
