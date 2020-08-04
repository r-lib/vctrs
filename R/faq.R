#' FAQ - How is the compatibility of vector types decided?
#'
#' @includeRmd man/faq/user/faq-compatibility-types.Rmd description
#'
#' @name faq-compatibility-types
NULL

#' FAQ - Error/Warning: Some attributes are incompatible
#'
#' @description
#'
#' This error occurs when [vec_ptype2()] or [vec_cast()] are supplied
#' vectors of the same classes with different attributes. In this
#' case, vctrs doesn't know how to combine the inputs.
#'
#' To fix this error, the maintainer of the class should implement
#' self-to-self coercion methods for [vec_ptype2()] and [vec_cast()].
#'
#' @includeRmd man/faq/developer/links-coercion.Rmd
#'
#' @name faq-error-incompatible-attributes
NULL
