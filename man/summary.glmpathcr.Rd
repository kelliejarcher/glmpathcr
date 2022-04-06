\name{summary.glmpathcr}
\alias{summary.glmpathcr}
\title{
Summarize a glmpathcr Object
}
\description{
This function summarizes the fits of \code{glmpathcr} over a path and returns a matrix that includes the degrees of freedom, deviance, AIC, and BIC associated with each model fit.
}
\usage{
\method{summary}{glmpathcr}(object, ...)
}
\arguments{
  \item{object}{a glmpathcr object}
  \item{\dots}{additional arguments}
}
\value{
A matrix is returned with the following columns corresponding to each model fit (rows):
\item{Df}{degrees of freedom at each step}
\item{Deviance}{deviance computed at each step}
\item{AIC}{AIC value at each step}
\item{BIC}{BIC value at each step}
}
\references{
Mee Young Park and Trevor Hastie (2007) L1 regularization path algorithm for generalized linear models. J. R. Statist. Soc. B, 69, 659-677.

Archer K.J., Williams A.A.A. (2012) L1 penalized continuation ratio models for ordinal response prediction using high-dimensional datasets. \emph{Statistics in Medicine}, 31(14), 1464-74.
}
\author{
Kellie J. Archer \email{archer.43@osu.edu}
}
\note{
This function is essentially the same as \code{summary.glmpath} from the \pkg{glmpath} package by Park and Hastie but was designed to operate on a returned \code{glmpathcr} object.
}

\seealso{
See Also as \code{\link{predict.glmpathcr}}, \code{\link{coef.glmpathcr}}
}
\examples{
data(diabetes)
x <- diabetes[, 2:dim(diabetes)[2]]
y <- diabetes$y
fit <- glmpathcr(x, y)
summary(fit)
}
\keyword{ methods }