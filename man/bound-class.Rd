\docType{class}
\name{bound-class}
\alias{abparms}
\alias{abparms-class}
\alias{bound}
\alias{bound-class}
\alias{boundNBF}
\alias{boundNBF-class}
\alias{boundEst}
\alias{boundEst-class}



\title{Classes for binary sequential boundaries}
\description{
  There are several classes that represent binary
  sequential boundaries. The most simple is the abparms
  class, then comes the bounds class (which contains
  abparms), then comes boundNBF class (which contains bound
  class), or boundEst class (which contains bounds class),
  then comes boundNBFEst (which contains boundNBF).  See
  details for which slots go with which classes.
}
\section{Slots}{
  \describe{ 
\item{\code{Nk}:}{vector of number of samples
  at boundary stopping points} 
\item{\code{a}:}{ vector for
  lower bound, stop if S out of Nk is less than or equal a.
  NA denotes do not stop.} 
\item{\code{b}:}{vector for
  upper bound, stop if S out of Nk is greater than or equal
  b. NA denotes do not stop.}
\item{\code{binding}:}{character specifying which
  boundary section is binding, either 'both', 'upper', or
  'lower'} 
 \item{\code{alternative}:}{character specifying alternative, 'two.sided', 'less', or 'greater'}
  \item{\code{N}:}{vector of number of samples at boundary stopping
  points}
  \item{\code{S}:}{vector of number of sucesses at boundary
  stopping points}
  \item{\code{K}:}{vector of number of ways to get to each boundary
  point}
  \item{\code{order}:}{vector of ordering of points}
  \item{\code{UL}:}{character vector denoting part of boundary,
  either 'lower' or 'upper' or 'end'}
 \item{\code{estimate}:}{vector of estimates of theta, probability of success}
    \item{\code{lower}:}{vector of lower confidence intervals for theta}
    \item{\code{upper}:}{vector of upper confidence intervals for theta}
    \item{\code{conf.level}:}{confidence level associated with confidence intervals}
    \item{\code{alpha}:}{error on either side}
    \item{\code{theta0}:}{null value for theta}
    \item{\code{plower}:}{vector of lower p-values}
    \item{\code{pupper}:}{vector of upper p-values}
    \item{\code{pval}:}{vector of p-values as directed by alternative slot}
   }
}
\details{
  The simplest representation of a binary sequential
  boundary is the abparms class, represented by a vector of
  the total number of trials (Nk) where to stop, and denoting
  stopping when number of sucesses, S, is S>=b or S<= a.
  One sided boundaries can be represented by all NA values
  for either a or b. Often times a two-sided boundary
  treats one side as a superiority boundary which must be
  stopped if crossed (a binding boundary), while the other
  side of the boundary is a futility boundary which may be
  ignored (a non-binding boundary). For example when
  binding='upper', then p-values for the upper boundary are
  calculated as if the lower boundaries are ignored if
  crossed and stopping happens on the lower side at max(Nk)
  instead, while the p-values for the lower and end
  boundary points are calculated using all (lower,upper and
  end) boundaries. \cr \cr Next is the bound class which
  adds the slots N (number of trials at each boundary
  point), S (number of sucesses at each point), K (number
  of ways to get to each point), order (ordering of points
  for p-value calculations), UL('upper','lower' or 'end').
}
\section{Methods}{
There is a \code{plot} and a \code{points} method for \code{boundEst} objects. 
}
\examples{
new("abparms",Nk=200)
}
\author{ Jenn Kirk, Michael P. Fay}