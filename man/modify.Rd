\name{modify}
\alias{modify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Modify binary sequential boundary
}
\description{
Modify several different aspects of a binary sequential boundary. 
Most modifications do not change the stopping boundaries. The  exceptions are 'missN', which allows modifications for missing assessments,
and 'closeout', which allows for early stopping of the trial for administrative reasons (i.e., reasons that do not depend on the responses in the trial). 
Other modifications possible: level of the confidence intervals (using tsalpha, conf.level, or alternative), which boundaries are binding (i.e., can change from a boundary with binding futility boundaries to one with 
non-binding futility boundaries), null hypothesis value (theta0), and whether the confidence intervals should match the non-binding futility p-values on the superiority boundaries (cipMatch).  
}
\usage{
modify(b, missN = NULL, theta0 = NULL, tsalpha = NULL,
    conf.level = NULL, alternative = NULL, cipMatch = TRUE,
    binding = NULL, closeout=NULL,  ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{
an object of class \code{\link{boundEst}} 
}
  \item{missN}{
a vector of missed assessments
}
  \item{theta0}{
null hypothesis probability of success
}
  \item{tsalpha}{
vector of length 2 with nominal significance level, if not NULL overrides \code{conf.level} and \code{alternative}
}
  \item{conf.level}{
confidence level, ignored if tsalpha is not NULL
}
  \item{alternative}{
character, alternative hypothesis, either 'less', 'greater' or 'two.sided' 
}
  \item{cipMatch}{
logical, for non-binding futility boundaries, should CI match the p-values on the binding boundary
}
  \item{binding}{character, which sides are binding: 'both', 'upper', or 'lower'}
  \item{closeout}{total number of trials at early closeout}
  \item{\dots}{
other parameters passed
}
}
%\details{%%  ~~ If necessary, more details than the description above ~~}
\value{an object of class \code{\link{boundEst}} }
%\references{}
%\author{}
%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{}
\examples{
b<-designOBF(50)
bmod<-modify(b,missN=30:36)
par(mfrow=c(2,1))
plot(b)
plot(bmod)
}
\keyword{htest}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
