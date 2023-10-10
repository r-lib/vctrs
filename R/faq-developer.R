#' FAQ - Is my class compatible with vctrs?
#'
#' @includeRmd man/faq/developer/reference-compatibility.Rmd description
#'
#' @name reference-faq-compatibility
NULL

#' FAQ - How does coercion work in vctrs?
#'
#' @includeRmd man/faq/developer/theory-coercion.Rmd description
#'
#' @name theory-faq-coercion
NULL

# The `@name` must be `vector_recycling_rules` to ensure a pkgdown page
# continues to exist for `vector_recycling_rules` since other packages link to
# this page and currently `@aliases` don't get a page on the pkgdown
# site due to: https://github.com/r-lib/pkgdown/issues/1876
#' FAQ - How does recycling work in vctrs and the tidyverse?
#'
#' @includeRmd man/faq/developer/theory-recycling.Rmd description
#'
#' @name vector_recycling_rules
#' @aliases theory-faq-recycling
NULL

#' FAQ - How to implement ptype2 and cast methods?
#'
#' @includeRmd man/faq/developer/howto-coercion.Rmd description
#'
#' @name howto-faq-coercion
NULL

#' FAQ - How to implement ptype2 and cast methods? (Data frames)
#'
#' @includeRmd man/faq/developer/howto-coercion-data-frame.Rmd description
#'
#' @name howto-faq-coercion-data-frame
NULL

#' FAQ - Why isn't my class treated as a vector?
#'
#' @includeRmd man/faq/developer/howto-faq-fix-scalar-type-error.Rmd description
#'
#' @name howto-faq-fix-scalar-type-error
NULL
