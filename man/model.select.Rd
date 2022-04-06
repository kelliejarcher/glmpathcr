\name{model.select}
\alias{model.select}
\title{
Step of Optimal Fitted AIC or BIC CR Model.
}
\description{
This function returns the step number associated with the minimum AIC and BIC models for a \code{glmpathcr} object and so can be used in selecting the step number for calls to functions such as \code{coef.glmpathcr} and \code{predict.glmpathcr}.
}
\usage{
model.select(object, which = "BIC")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a \code{glmpathcr} object}
  \item{which}{determines whether the step is associated with the optimal \code{which="BIC"} model or \code{which="AIC"} model}
}

\value{
A numeric value of length one representing the step number
}
\author{
Kellie J. Archer, \email{archer.43@osu.edu}
}

\seealso{
See also as \code{\link{coef.glmpathcr}}, \code{\link{predict.glmpathcr}}
}
\examples{
data(diabetes)
x <- diabetes[, 2:dim(diabetes)[2]]
y <- diabetes$y
fit <- glmpathcr(x, y)
model.select(fit, which="AIC")
}
\keyword{ misc }