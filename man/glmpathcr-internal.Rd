\name{glmpathcr-internal}
\alias{glmpathcr-internal}
\alias{print.glmpathcr}
\alias{cr.backward}
\alias{cr.forward}
\title{
Functions called by glmpathcr functions, not by the user
}
\description{
These functions are called my other \code{glmpathcr} functions and are not intended to be directly called by the user.
}
\usage{
cr.backward(x,y,weight)
cr.forward(x,y,weight)
\method{print}{glmpathcr}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a matrix of predictor variables}
  \item{y}{the ordinal response to be predicted}
  \item{weight}{an optional vector of weights for observations}
  \item{fit}{a fitted \code{glmpathcr} object}
}
\details{
The \code{cr.backward} and \code{cr.forward} functions restructure the original dataset to represent the K-1 conditionally independent datasets needed for the full likelihood.
}
\references{
Bender & Benner and O'Connell for backward CR programming
}
\author{
Kellie J. Archer, \email{archer.43@osu.edu}
}

\seealso{
See Also as \code{\link{glmpathcr}}.
}

\keyword{ internal }