expect_dim <- function(x, shape) {
  dim <- dim2(x)
  expect_equal(dim, !!shape)
}

expect_lossy <- function(expr, result, x = NULL, to = NULL) {
  expr <- enquo(expr)
  expect_error(eval_tidy(expr), class = "vctrs_error_cast_lossy")

  out <- allow_lossy_cast(eval_tidy(expr), x_ptype = x, to_ptype = to)
  expect_identical(!!out, !!result)
}

expect_args <- function(x, y, x_arg, y_arg) {
  err <- catch_cnd(vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg), classes = "vctrs_error_incompatible_type")
  expect_true(!is_null(err))

  expect_true(grepl(paste0("for `", x_arg, "`"), err$message, fixed = TRUE))
  expect_true(grepl(paste0("and `", y_arg, "`"), err$message, fixed = TRUE))

  expect_identical(list(err$x_arg, err$y_arg), list(x_arg, y_arg))
}

# Work around deparsing of !! on old versions of R
as_label2 <- function(expr) {
  expr <- duplicate(expr, shallow = FALSE)

  label <- as_label(fix_bang(expr))
  label <- gsub("+++", "!!!", label, fixed = TRUE)
  label <- gsub("++", "!!", label, fixed = TRUE)
  label
}
fix_bang <- function(expr) {
  curr <- expr
  while (!is_null(curr)) {
    car <- node_car(curr)

    if (is_triple_bang(car)) {
      replace_triple_bang(car)
    } else if (is_double_bang(car)) {
      replace_double_bang(car)
    } else if (is_call(car)) {
      node_poke_car(curr, fix_bang(car))
    }

    curr <- node_cdr(curr)
  }

  expr
}
is_double_bang <- function(expr) {
  is_call(expr, "!") && is_call(node_cadr(expr), "!")
}
is_triple_bang <- function(expr) {
  is_double_bang(expr) && is_call(node_cadr(node_cadr(expr)), "!")
}
replace_double_bang <- function(expr) {
  node_poke_car(expr, sym("+"))
  node_poke_car(node_cadr(expr), sym("+"))
}
replace_triple_bang <- function(expr) {
  replace_double_bang(expr)
  node_poke_car(node_cadr(node_cadr(expr)), sym("+"))
}

try2 <- function(expr) {
  cat(paste0("\n", as_label2(substitute(expr)), ":\n\n"))
  cat(catch_cnd(expr, classes = "error")$message, "\n\n")
}

expect_known_output_nobang <- function(object, file, ...) {
  expect_known_output(object, file, ...)
}

expect_syntactic <- function(name, exp_syn_name) {
  expect_identical(
    syn_name <- make_syntactic(name),
    exp_syn_name
  )
  expect_identical(syn_name, make.names(syn_name))
}

expect_error_cnd <- function(object, class, message = NULL, ..., .fixed = TRUE) {
  cnd <- expect_error(object, regexp = message, class = class, fixed = .fixed)
  expect_true(inherits_all(cnd, class))

  exp_fields <- list2(...)
  expect_true(is_empty(setdiff(!!names(exp_fields), names(cnd))))
  expect_equal(cnd[names(exp_fields)], exp_fields)
}
