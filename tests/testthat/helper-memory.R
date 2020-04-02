maybe_referenced_col <- function(x, i) {
  .Call(vctrs_maybe_referenced_col, x, i)
}

new_df_unreferenced_col <- function() {
  .Call(vctrs_new_df_unreferenced_col)
}
