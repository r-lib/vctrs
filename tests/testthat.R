library(testthat)
library(vctrs)

if (requireNamespace("xml2")) {
  test_check("vctrs", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("vctrs")
}
