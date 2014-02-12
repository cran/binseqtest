\name{powerBsb}
\alias{powerBsb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Power for binary sequential boundary
}
\description{Calculate power from \code{\link{boundEst}} object for vector of alternatives}
\usage{
powerBsb(object, theta = 0.6, alternative = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a 'boundEst' object}
  \item{theta}{vector of theta values, probability of success}
  \item{alternative}{character, either 'two.sided', 'less', or 'greater'}
}
\details{
Power to reject. For alternative='greater' reject when pU<tsalpha['alphaUpper'], and
for alternative='less' reject when pL<tsalpha['alphaLower']. For 
alternative='two.sided' if theta[i]>theta0 reject when pU<tsalpha['alphaUpper'],
if theta[i]<theta0 reject when pL<tsalpha['alphaLower'],  if 
theta[i]==theta0 and tsalpha['alphaUpper']<=tsalpha['alphaLower'] reject when pU<tsalpha['alphaUpper'], and  if 
theta[i]==theta0 and tsalpha['alphaUpper']>tsalpha['alphaLower'] reject when pL<tsalpha['alphaLower']. 
 
}
\value{
a vector with power associated with the parameter vector theta
}
%\references{%% ~put references to the literature/web site here ~}
%\author{%%  ~~who you are~~}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{%% ~~objects to See Also as \code{\link{help}}}
\examples{
B<-designAb(Nk=c(20,40),a=10,theta0=.4)
powerBsb(B,theta=c(.1,.4,.8),alternative="less")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
