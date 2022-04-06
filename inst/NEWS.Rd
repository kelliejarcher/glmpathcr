\name{NEWS}
\title{News for Package glmpathcr}
\section{Changes in version 1.0.4}{
\itemize{
  \item Change syntax to remove the period from all relevant functions. The main modeling fitting function is now glmpathcr which yields an object of class glmpathcr.
  \item Other function name changes are: coef.glmpathcr, fitted.glmpathcr, plot.glmpathcr, predict.glmpathcr, print.glmpathcr, and summary.glmpathcr.
  \item glmpathcr function now handles weights.
  \item Housekeeping: Cleaned up the DESCRIPTION, NAMESPACE, and URLs in help files.
  }
}
\section{Changes in version 1.0.5}{
\itemize{
  \item Updated package author's and package maintainer's email address.
}
}
\section{Changes in version 1.0.6}{
\itemize{
  \item Edited glmpathcr-package.Rd to be in sync with the rest of the package.
}
}
\section{Changes in version 1.0.7}{
\itemize{
  \item Added nopenalty parameter to glmpathcr fitting function so a user can indicate the indices of predictor variables that should not be penalized.
  \item Added a check for ordered factor outcome in the predict function.
  \item Added a check to ensure number of steps and row dimension of coefficients match for trace plot.
}
}
\section{Changes in version 1.0.8}{
\itemize{
  \item Updated functions that check for class(object)=="matrix" to comply with new results in R 4.0.0 that yield a length 2 vector with "matrix" and "array".
  \item Updated glmpathcr function to accept user-defined parameters allowable in glmpath call (e.g., max.steps, max.vars, trace, etc) except offset.  
}
}

