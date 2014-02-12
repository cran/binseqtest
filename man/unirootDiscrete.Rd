\name{unirootDiscrete}
\alias{unirootDiscrete}
\title{Identify where a non-increasing function changes sign}
\description{
  Let f be a non-increasing (or non-decreasing) function that changes sign within the
     interval specified.  If 'pos.side'=TRUE (or FALSE) then
     \code{unirootDiscrete} finds the value x such that  f(x) is closest
     to the sign change and is positive (or negative).
}
\usage{
unirootDiscrete(f, interval, lower = min(interval), 
    upper = max(interval), tol = 10^-5, pos.side = FALSE,
    print.steps = FALSE, maxiter = 1000, ...)
}
\arguments{
  \item{f}{ function for which a root is needed }
  \item{interval}{an interval giving minimum and maximum allowable values for root }
  \item{lower}{ lower bound for root }
  \item{upper}{ upper bound for root }
  \item{tol}{ absolute tolerance, abs(true root-estimated root)<= tol}
  \item{pos.side}{if TRUE finds value x closest to the sign change in f, such that f(x)>0}
  \item{print.steps}{ if true prints interations}
  \item{maxiter}{maximum number of iterations}
  \item{\dots}{additional arguments to f }
}
\details{
  The algorithm evaluates f(x) iteratively, and the change in  'x' is halved each
  iteration until the change in 'x' is less than tol. Then the root is returned according to 
  the pos.side parameter.
}
\value{
 A list with the folllowing elements,
  \item{iter}{number of iterations (times f is evaluated)}
  \item{f.root}{value of f(x), where x is the root}
  \item{root}{the root x, where f(x)>=0 if pos.side=TRUE}
  ...
}
%\references{ ~put references to the literature/web site here ~ }
\author{ M.P. Fay }
%\note{ ~~further notes~~  ~Make other sections like Warning with \section{Warning }{....~}
%\seealso{See also \code{\link{uniroot.integer}}}
\examples{
test<-function(x,parm=10.987654321){ ifelse(x>=parm,1,-1) }
unirootDiscrete(test,lower=0,upper=100,tol=10^-4,pos.side=FALSE,print.steps=TRUE)
}
\keyword{math}% at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line