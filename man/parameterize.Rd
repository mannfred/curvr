% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_formatting.R
\name{parameterize}
\alias{parameterize}
\title{Parameterize polynomial objects from \code{Momocs}.}
\usage{
parameterize(poly)
}
\arguments{
\item{poly}{A \code{list} object generated by \code{Momocs::npoly()}
or \code{Momocs::opoly()}. This function does not currently support \code{OpnCoe} objects.}
}
\value{
a polynomial function (eg y = x^2) of the form
\code{function(t) {t, t^2}}.
}
\description{
Convert polynomial objects from \code{Momocs} into t-parameterized
polynomials readable by \code{pracma::arclength()}.
}
\examples{

library(Momocs)
library(pracma)

x <- 1:10
y <- x^2

# a fictional landmark dataset
mdat <- matrix(c(x,y), nrow=10, ncol=2)

# fit a second-order polynomial to the landmarks
my_poly <- Momocs::npoly(mdat, 2)

# parameterize by t
paramd_poly <- parameterize(my_poly)

# calculate arc length between x=0 and x=1
pracma::arclength(paramd_poly, 0, 1)
}
\seealso{
\code{\link{as_expression}} for converting Momocs polynomials to expressions
and \code{\link{as_function}} for creating R functions.
}
