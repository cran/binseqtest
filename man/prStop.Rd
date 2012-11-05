\name{prStop}
\alias{prStop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probabilty of Stopping
}
\description{
Calculates the probability of stopping at any point in a binary sequential boundary.
}
\usage{
prStop(object, theta = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{\link{boundEst}}  
}
  \item{theta}{
probability of a positive response
}
}
%\details{}
\value{
A list with the following elements
\item{B}{the \code{boundEst} object inputted}
\item{Nupper}{ vector of N values for stopping on upper boundary}
\item{dStopUpper}{ vector of probabilities for stopping at each value of Nupper}
\item{pStopUpper}{ vector of cumulative probabilities for stopping by each value of Nupper}
\item{Nlower}{ vector of N values for stopping on lower boundary}
\item{dStopLower}{ vector of probabilities for stopping at each value of NLower}
\item{pStopLower}{ vector of cumulative probabilities for stopping by each value of NLower}
\item{Nend}{N value at the end of the boundary, i.e., max(N) }
\item{dStopEnd}{probability of stopping at the end of the boundary}
\item{check}{check value, should be 1 or very close to it}
}
%\references{}
%\author{%%  ~~who you are~~}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{powerBsb}}}
\examples{
b<-designOBF(20,theta0=.5)
prStop(b,theta=.5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
