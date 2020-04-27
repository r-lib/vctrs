maybe_shared_col <- function(x, i) {
  .Call(vctrs_maybe_shared_col, x, i)
}

new_df_unshared_col <- function() {
  .Call(vctrs_new_df_unshared_col)
}
