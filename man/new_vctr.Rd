% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type-vctr.R
\name{new_vctr}
\alias{new_vctr}
\alias{vctr}
\title{vctr (vector) S3 class}
\usage{
new_vctr(.data, ..., class = character(), inherit_base_type = NULL)
}
\arguments{
\item{.data}{Foundation of class. Must be a vector}

\item{...}{Name-value pairs defining attributes}

\item{class}{Name of subclass.}

\item{inherit_base_type}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
A single logical, or \code{NULL}. Does this class extend the base type of
\code{.data}? i.e. does the resulting object extend the behaviour of the
underlying type? Defaults to \code{FALSE} for all types except lists, which
are required to inherit from the base type.}
}
\description{
This abstract class provides a set of useful default methods that makes it
considerably easier to get started with a new S3 vector class. See
\code{vignette("s3-vector")} to learn how to use it to create your own S3
vector classes.
}
\details{
List vctrs are special cases. When created through \code{new_vctr()}, the
resulting list vctr should always be recognized as a list by
\code{obj_is_list()}. Because of this, if \code{inherit_base_type} is \code{FALSE}
an error is thrown.
}
\section{Base methods}{

The vctr class provides methods for many base generics using a smaller
set of generics defined by this package. Generally, you should think
carefully before overriding any of the methods that vctrs implements for
you as they've been carefully planned to be internally consistent.
\itemize{
\item \code{[[} and \code{[} use \code{NextMethod()} dispatch to the underlying base function,
then restore attributes with \code{vec_restore()}.
\code{rep()} and \verb{length<-} work similarly.
\item \verb{[[<-} and \verb{[<-} cast \code{value} to same type as \code{x}, then call
\code{NextMethod()}.
\item \code{as.logical()}, \code{as.integer()}, \code{as.numeric()}, \code{as.character()},
\code{as.Date()} and \code{as.POSIXct()} methods call \code{vec_cast()}.
The \code{as.list()} method calls \code{[[} repeatedly, and the \code{as.data.frame()}
method uses a standard technique to wrap a vector in a data frame.
\item \code{as.factor()}, \code{as.ordered()} and \code{as.difftime()} are not generic functions
in base R, but have been reimplemented as generics in the \code{generics}
package. \code{vctrs} extends these and calls \code{vec_cast()}. To inherit this
behaviour in a package, import and re-export the generic of interest
from \code{generics}.
\item \code{==}, \code{!=}, \code{unique()}, \code{anyDuplicated()}, and \code{is.na()} use
\code{\link[=vec_proxy]{vec_proxy()}}.
\item \code{<}, \code{<=}, \code{>=}, \code{>}, \code{min()}, \code{max()}, \code{range()}, \code{median()},
\code{quantile()}, and \code{xtfrm()} methods use \code{\link[=vec_proxy_compare]{vec_proxy_compare()}}.
\item \code{+}, \code{-}, \code{/}, \code{*}, \code{^}, \code{\%\%}, \code{\%/\%}, \code{!}, \code{&}, and \code{|} operators
use \code{\link[=vec_arith]{vec_arith()}}.
\item Mathematical operations including the Summary group generics (\code{prod()},
\code{sum()}, \code{any()}, \code{all()}), the Math group generics (\code{abs()}, \code{sign()},
etc), \code{mean()}, \code{is.nan()}, \code{is.finite()}, and \code{is.infinite()}
use \code{\link[=vec_math]{vec_math()}}.
\item \code{dims()}, \verb{dims<-}, \code{dimnames()}, \verb{dimnames<-}, \code{levels()}, and
\verb{levels<-} methods throw errors.
}
}

\keyword{internal}
