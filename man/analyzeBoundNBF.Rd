\name{analyze}
\alias{analyzeBoundNBF}
\alias{analyzeBound}
\alias{analyze}
\title{
Methods for calculating estimates, confidence intervals, and p-values from binary sequential boundaries
}
\description{
For routine use, these functions will not need to be called directly but are called from within the design functions (see \code{\link{designOBF}}). If needed, use \code{analyze} for any class representing a binary sequential boundary (see \code{\link{bound-class}}),
and the appropriate function is called.
}
\usage{
analyzeBound(object, theta0 = 0.5, stats = "all", 
    alternative = "two.sided", conf.level = 0.95, 
    tsalpha = NULL, ...)
analyzeBoundNBF(object, theta0 = 0.5, stats = "all", 
    alternative = "two.sided", conf.level = 0.95, 
    tsalpha = NULL, cipMatch = TRUE, ...)
}
\arguments{
  \item{object}{a binary sequential boundary (for classes see \code{\link{bound-class}})}
  \item{theta0}{probability of success under the null}
  \item{stats}{character, either 'all' or 'pval'}
  \item{tsalpha}{vector of length 2, error on either side, if present overrides alternative and conf.level (see details)}
  \item{alternative}{character, either 'two.sided', 'less', or 'greater'}
  \item{conf.level}{confidence level}
  \item{cipMatch}{logical, for non-binding futility boundaries, should CI match the p-values on the binding boundary}
  \item{\dots}{further arguments to be passed}
}
%\details{}
\value{if stats='all' returns an object of class 'boundEst', otherwise returns a numeric vector of p-values}
%\references{}
%\author{%%  ~~who you are~~}
%\note{%%  ~~further notes~~}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
See  \code{\link{analyze-methods}}
}
%\examples{ }
%\keyword{ ~kwd1 }