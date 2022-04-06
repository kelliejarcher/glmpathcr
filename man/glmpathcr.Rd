\name{glmpathcr}
\alias{glmpathcr}
\title{
Fit Penalized Continuation Ratio Model
}
\description{
This function fits a penalized backward (default) or forward continuation ratio model using the glmpath framework.}
\usage{
glmpathcr(x, y, data, method="backward", weight = rep(1, n), offset = rep(0, n), 
lambda2 = 1e-05, max.steps = 10 * min(n, m), max.norm = 100 * m, 
min.lambda = (if (m >= n) 1e-06 else 0), max.vars = Inf, max.arclength = Inf, 
frac.arclength = 1, add.newvars = 1, bshoot.threshold = 0.1, relax.lambda = 1e-08, 
standardize = TRUE, function.precision = 3e-13, eps = .Machine$double.eps, trace = FALSE, 
nopenalty=NULL)
}
\arguments{
  \item{x}{a matrix of predictor variables}
  \item{y}{ordinal response}
  \item{data}{optional; list that includes \code{x} and \code{y} components}
  \item{method}{select between fitting a backward (default) versus a forward continuation ratio model}
  \item{weight}{an optional vector of weights for observations}
  \item{offset}{an optional vector of offset. If a column of \code{x} is used as offset, the corresponding column must be removed from \code{x}}
  \item{lambda2}{regularization parameter for the L2 norm of the coefficients. Default is \code{1e-5}.}
  \item{max.steps}{an optional bound for the number of steps to be taken. Default is \code{10 * min{nrow(x), ncol(x)}}}
  \item{max.norm}{an optional bound for the L1 norm of the coefficients. Default is \code{100 * ncol(x)}}
  \item{min.lambda}{an optional (lower) bound for the size of \eqn{\lambda}. Default is 0 for \code{ncol(x) < nrow(x)} cases and \code{1e-6} otherwise}
  \item{max.vars}{an optional bound for the number of active variables. Default is \code{Inf}}
  \item{max.arclength}{an optional bound for arc length (L1 norm) of a step. If \code{max.arclength} is extremely small, an exact nonlinear path is produced. Default is \code{Inf}}
  \item{frac.arclength}{Under the default setting, the next step size is computed so that the active set changes right at the next value of \eqn{\lambda}. When \code{frac.arclength} is assigned some fraction between 0 and 1, the step size is decreased by the factor of \code{frac.arclength} in arc length. If \code{frac.arclength=0.2}, the step length is adjusted so that the active set would change after five smaller steps. Either \code{max.arclength} or \code{frac.arclength} can be used to force the path to be more accurate. Default is \code{1}.}
  \item{add.newvars}{\code{add.newvars} candidate variables (that are currently not in the active set) are used in the corrector step as potential active variables. Default is \code{1}.}
  \item{bshoot.threshold}{If the absolute value of a coefficient is larger than \code{bshoot.threshold} at the first corrector step it becomes nonzero (therefore when \eqn{\lambda} is considered to have been decreased too far), \eqn{\lambda} is increased again. i.e. A backward distance in \eqn{\lambda} that makes the coefficient zero is computed. Default is \code{0.1}}
  \item{relax.lambda}{A variable joins the active set if |l(\eqn{\beta})| > \eqn{\lambda}*(1-relax.lambda). Default is \code{1e-8}. If no variable joins the active set even after many (>20) steps, the user should increase \code{relax.lambda} to \code{1e-7} or \code{1e-6}, but not more than that. This adjustment is sometimes needed because of the numerical precision/error propagation problems. In general, the paths are less accurate with relaxed lambda.}
  \item{standardize}{If \code{TRUE}, predictors are standardized to have a unit variance}
  \item{function.precision}{\code{function.precision} parameter used in the internal solver. Default is \code{3e-13}. The algorithm is faster, but less accurate with relaxed, larger function precision}
  \item{eps}{effective zero}
  \item{trace}{If \code{TRUE}, the algorithm prints out its progress}
  \item{nopenalty}{a set of indices for the predictors that are not subject to the L1 penalty}
}
\details{
The \code{glmpathcr} function is largely borrowed from the \pkg{glmpath} package and differs only in that (1) the ordinal dataset is first restructured to represent the K-1 conditionally independent likelihoods and (2) the family is specified to be binomial and the \code{nopenalty.subset} is specified to be the thresholds for the ordinal classes.
}
\value{
A \code{glmpathcr} object is returned.
\item{lambda}{vector of \eqn{\lambda} values for which the exact coefficients are computed}
\item{lambda2}{\eqn{\lambda_2} used}
\item{step.length}{vector of step lengths in \eqn{\lambda}}
\item{core}{matrix of l(\eqn{\beta}) values (derivatives of the log-likelihood)}
\item{new.df}{vector of degrees of freedom (to be used in the plot function)}
\item{df}{vector of degrees of freedom at each step}
\item{deviance}{vector of deviance computed at each step}
\item{aic}{vector of AIC values from fitted logistic regression on restructured data; use \code{summary} for AIC from continuation ratio model}
\item{bic}{vector of BIC values from fitted logistic regression on restructured data; use \code{summary} for AIC from continuation ratio model}
\item{b.predictor}{matrix of coefficient estimates from the predictor steps}
\item{b.corrector}{matrix of coefficient estimates from the corrector steps}
\item{new.A}{vector of boolean values indicating the steps at which the active set changed (to be used in the plot/predict functions)}
\item{actions}{actions taken at each step}
\item{means}{means of the columns of x}
\item{sdx}{standard deviations of the columns of x}
\item{xnames}{column names of x}
\item{family}{family used}
\item{weight}{weights used}
\item{offset}{offset used}
\item{nopenalty.subset}{nopenalty.subset used}
\item{standardize}{TRUE if the predictors were standardized before fitting}
\item{x}{the matrix of predictor variables used in fitting the model}
\item{y}{the ordinal outcome}
\item{method}{either \code{forward} of \code{backward} method}
}
\references{
Ralf Bender and Axel Benner (2000) Calculating ordinal regression models in SAS and S-Plus. Biometrical Journal 42, 677--699.
}
\author{
Kellie J. Archer
}
\note{
For further details about the fitting algorithm, see the \pkg{glmpath} package.
}

\seealso{
See also as \code{\link{predict.glmpathcr}}, \code{\link{coef.glmpathcr}}
}
\examples{
data(diabetes)
x <- diabetes[, 2:dim(diabetes)[2]]
y <- diabetes$y
fit <- glmpathcr(x, y)
}
\keyword{ models }
\keyword{ classif }
