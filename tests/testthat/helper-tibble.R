
expect_grouped <- function(x, groups, names = base::names(mtcars)) {
  expect_is(x, "grouped_df")
  expect_identical(dplyr::group_vars(x), groups)
  expect_named(x, names)
}
