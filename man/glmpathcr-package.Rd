\name{glmpathcr-package}
\alias{glmpathcr-package}
\docType{package}
\title{
\packageTitle{glmpathcr}: Penalized L1 continuation Ratio Models for Ordinal Response Prediction in High-dimensional Data Settings
}
\description{
This package provides a function glmpathcr for fitting a penalized L1 continuation ratio model for predicting an ordinal response and associated methods for plotting, getting predicted values, estimating coefficients for selected models.
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{glmpathcr}
\packageIndices{glmpathcr}
This package contains functions for fitting a penalized continuation ratio model and extracting estimated coefficients, predicted class, and fitted probabilities. The model and methods can be used when the response to be predicted is ordinal, and is particularly relevant when there are more covariates than observations.
}
\author{
\packageAuthor{glmpathcr} Kellie J. Archer <archer.43@osu.edu>

Maintainer: \packageMaintainer{glmpathcr} Kellie J. Archer <archer.43@osu.edu>
}
\references{
Archer K.J., Williams A.A.A. (2012) L1 penalized continuation ratio models for ordinal response prediction using high-dimensional datasets. \emph{Statistics in Medicine}, 31(14), 1464-74.
}
\keyword{ package }
\keyword{ models }
\keyword{ classif }
\seealso{
 See also \code{\link[glmpath:glmpath]{glmpath}}
}
\examples{
   data(diabetes)
   x <- diabetes[, 2:dim(diabetes)[2]]
   y <- diabetes$y
   fit <- glmpathcr(x, y)
}
