\name{analyze-methods}
\docType{methods}
\alias{analyze-methods}
\alias{analyze,abparms-method}
\alias{analyze,ANY-method}
\alias{analyze,bound-method}
\alias{analyze,boundNBF-method}
\title{Calculate estimates, confidence intervals and p-values from binary sequential boundary}
\description{
The method \code{analyze} calculates the estimate, confidence interval and p-values (both one-sided ones and the two-sided one)
from a binary sequential boundary. The methods works on any of the classes that represent those boundaries (see \code{\link{bound-class}}).
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ANY")}}{
Generic function: see \code{\link{analyze}}
}

\item{\code{signature(object = "abparms")}}{
Calculate estimates, confidence intervals and p-values from 'abparms' object.
}


\item{\code{signature(object = "bound")}}{
Calculate estimates, confidence intervals and p-values from 'bound' object.
}

\item{\code{signature(object = "boundNBF")}}{
Calculate estimates, confidence intervals and p-values from 'boundNBF' object.
}
}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
