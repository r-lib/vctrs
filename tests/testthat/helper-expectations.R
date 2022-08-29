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

  expect_true(grepl(paste0("combine `", x_arg, "`"), err$message, fixed = TRUE))
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

expect_syntactic <- function(name, exp_syn_name) {
  expect_identical(
    syn_name <- make_syntactic(name),
    exp_syn_name
  )
  expect_identical(syn_name, make.names(syn_name))
}

expect_error_cnd <- function(object, class, message = NULL, ..., .fixed = TRUE) {
  cnd <- expect_error(object, regexp = message, class = class[[1]], fixed = .fixed)
  expect_true(inherits_all(cnd, class))

  exp_fields <- list2(...)
  expect_true(is_empty(setdiff(!!names(exp_fields), names(cnd))))
  expect_equal(cnd[names(exp_fields)], exp_fields)
}

expect_incompatible_df <- function(x, fallback) {
  if (is_true(peek_option("vctrs:::warn_on_fallback"))) {
    x <- expect_df_fallback_warning(x)
  }
  expect_identical(x, fallback)
}
# Never warns so we don't get repeat warnings
expect_incompatible_df_cast <- function(x, fallback) {
  expect_identical(x, fallback)
}

expect_df_fallback_warning <- function(expr) {
  suppressWarnings(expect_warning({{ expr }}, "falling back to (<data.frame>|<tibble>)"))
}
expect_df_fallback_warning_maybe <- function(expr) {
  if (is_true(peek_option("vctrs:::warn_on_fallback"))) {
    expect_warning({{ expr }}, "falling back to (<data.frame>|<tibble>)")
  } else {
    expr
  }
}

scrub_internal_error_line_number <- function(x) {
  # Because it varies by OS
  sub(pattern = "at line [[:digit:]]+", replacement = "at line <scrubbed>", x = x)
}
