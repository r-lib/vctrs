expect_drop <- function(x, value) {
  drop <- dplyr::group_by_drop_default(x)
  if (value) {
    expect_true(drop)
  } else {
    expect_false(drop)
  }
}
