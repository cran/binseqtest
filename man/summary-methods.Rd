\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,boundEst-method}
\title{Methods for Function \code{summary} in Package \pkg{binseqtest}}
\description{ Objects of class \code{\link[=boundEst-class]{boundEst}} have a \code{summary} method. It basically calls stopTable(object) }
\section{Methods}{
\describe{

\item{\code{signature(object = "ANY")}}{
Gives a summary of object, usually a little more calculations than associated with print or show
}

\item{\code{signature(object = "boundEst")}}{
calls \code{\link[=stopTable]{stopTable(object, output='print')}}
}
}}
\keyword{methods}
%\keyword{ ~~ other possible keyword(s) ~~ }
