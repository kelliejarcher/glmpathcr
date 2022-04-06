\name{predict.glmpathcr}
\alias{predict.glmpathcr}
\alias{fitted.glmpathcr}

\title{
Predicted Class and Fitted Probabilities from glmpathcr Object
}

\description{
Returns either the predicted class or the fitted probabilities from the penalized continuation ratio model from a \code{glmpathcr} object.
}

\usage{
\method{predict}{glmpathcr}(object, newx = NULL, which = "BIC", type = "class", ...)
\method{fitted}{glmpathcr}(object, newx = NULL, which = "BIC", type = "class", ...)
}

\arguments{
  \item{object}{a \code{glmpathcr} object}
  \item{newx}{a data matrix containing the predictor variables, if NULL, the original training data is used for getting model predictions}
  \item{which}{used for model selection, either the default \code{which}="BIC" or \code{which}="AIC"}
  \item{type}{used for selecting desired output, either the default \code{type}="class" or \code{type}="probes" for the fitted probabilities}
  \item{\dots}{additional optional arguments}
}

\value{
Either a vector of the predicted class labels or a matrix containing the fitted probabilities is returned.
}
\references{
Archer K.J., Williams A.A.A. (2012) L1 penalized continuation ratio models for ordinal response prediction using high-dimensional datasets. \emph{Statistics in Medicine}, 31(14), 1464-74.
}
\author{
Kellie J. Archer, \email{archer.43@osu.edu}
}

\seealso{
See also as \code{\link{glmpathcr}}, \code{\link{coef.glmpathcr}}.
}
\examples{
data(diabetes)
x <- diabetes[, 2:dim(diabetes)[2]]
y <- diabetes$y
fit <- glmpathcr(x, y)
pred <- predict(fit)
table(pred, y)
pred <- predict(fit, which="AIC", type="probs")
}
\keyword{ methods }