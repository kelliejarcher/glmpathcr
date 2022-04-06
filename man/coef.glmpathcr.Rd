\name{coef.glmpathcr}
\alias{coef.glmpathcr}
\title{
Extract All Model Coefficients
}
\description{
The \code{coef.glmpathcr} function returns all estimated coefficients for a \code{glmpathcr} fitted model, where the model selected is indicated by step s.  
}
\usage{
\method{coef}{glmpathcr}(object, s, weight = NULL, eps = .Machine$double.eps, ...)
}

\arguments{
  \item{object}{a \code{glmpathcr} object}
  \item{s}{the step at which the coefficient estimates are desired}
  \item{weight}{weights, if any}
  \item{eps}{effective zero}
  \item{\dots}{additional arguments}
}

\value{
A vector of estimated coefficients is returned. The \code{coef.glmpathcr} function returns all estimated coefficients for a glmpathcr fitted model, where the model selected is indicated by step s. 
}

\author{
Kellie J. Archer, \email{archer.43@osu.edu}
}

\seealso{
See also as \code{\link{glmpathcr}}, \code{\link{nonzero.coef}}
}

\examples{
data(diabetes)
x <- diabetes[, 2:dim(diabetes)[2]]
y <- diabetes$y
fit <- glmpathcr(x, y)
step <- model.select(fit, which="BIC")
\dontrun{coef(fit, s = step)}
}
\keyword{ methods }
