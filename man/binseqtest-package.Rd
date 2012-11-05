\docType{package}
\name{binseqtest-package}
\alias{binseqtest}
\alias{binseqtest-package}
\title{Binary sequential tests}
\description{
  Design and analyze binary sequential tests
}
\details{
  \tabular{ll}{ Package: \tab binseqtest\cr Type: \tab
  Package\cr Version: \tab 0.3\cr Date: \tab 2012-11-05\cr
  License: \tab GPL (>=2)\cr LazyLoad: \tab yes\cr }

  The package creates designs for testing a series of
  binary responses sequentially. It allows checking after
  every response, or grouped sequential tests. Gives exact
  confidence intervals and p-values. Has an option for
  non-binding futility boundaries.

There are functions for creating the binary sequential boundaries or binary grouped sequential boundaries  (see \code{\link{designOBF}}), 
creating tables of statistics (estimates, confidence intervals, and p-values) at 
specific stopping points in the boundary (see \code{link{stopTable}}), modifying the boundaries (see \code{\link{modify}}), 
and plotting the boundaries (\code{\link{plot-methods}}).
}
\examples{
# create an O'Brien-Fleming-type design, with 2.5 percent error on each side with max N of 50
B<-designOBF(50)
# plot it
plot(B)
# create a table for N (total samples) values between 20 and 25
stopTable(B,Nrange=c(20,25))
# modify the boundary if you missed looks at N=30 through 35
Bmod<-modify(B,missN=30:35)
plot(Bmod)
}
\author{
  Jenn Kirk, Mike Fay
}
\keyword{package}

