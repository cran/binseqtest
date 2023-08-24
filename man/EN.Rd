\name{EN}
\alias{EN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Expected sample size for boundary.
}
\description{Calculate expected sample size for \code{\link[=bound-class]{bound}} object, after inputing theta. }
\usage{
EN(object, theta = 0.6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a object representing a binary sequential class (\code{\link{bound-class}})}
  \item{theta}{a vector of parameters representing the probability of a success}
}
%\details{%%  ~~ If necessary, more details than the description above ~~}
\value{
a vector of expected sample sizes associated with the \code{theta} argument.
}
%\references{%% ~put references to the literature/web site here ~}
%\author{%%  ~~who you are~~}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also \code{\link{powerBsb}}
}
\examples{
B<-designAb(Nk=c(20,40),a=c(5),b=c(15),theta0=.5)
En<-EN(B,theta=c(.1,.5,.6))
En
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
