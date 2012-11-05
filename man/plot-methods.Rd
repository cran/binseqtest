\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,boundEst,ANY-method}
\alias{plot,boundEst,missing-method}
\title{Methods for Function \code{plot} in Package \pkg{binseqtest}}
\description{
  Plot binary sequential boundaries for \code{"boundEst"} objects. 
}
\usage{
\S4method{plot}{boundEst,missing}(x, 
    rcol = c(orange = "#E69F00", skyBlue = "#56B4E9", greenBlue = "#009E73"), 
    rpch = c(1, 16, 16), 
    bplottype = "NS",   
    newplot = TRUE, dtext=NULL, grid=50, xlab=NULL, ylab=NULL, \dots)
}
\arguments{
 \item{x}{an object of class \code{"boundEst"} }
\item{rcol}{rejection color vector, rcol[1]=fail to reject, rcol[2]=reject, conclude theta>theta0, rcol[3]=reject, conclude theta< theta0 (see details)}
\item{rpch}{rejection pch vector, correspond to same categories as rcol vector}
\item{bplottype}{character, either 'NS' (default), 'FS', 'NB', 'NZ', or 'NE' (see details)}
\item{newplot}{logical, should a new plot be started? if FALSE add to existing plot (only makes sense to add to plot with the same bplottype)} 
\item{dtext}{logical, add descriptive text? if NULL only adds text when newplot=TRUE (used for bplottype='NS' or 'FS')} 
\item{grid}{numeric, if maximum possible total trials<=grid then add gridlines (used for bplottype='NS' or 'FS')} 
\item{xlab}{title for x axis, if NULL value depends on bplottype} 
\item{ylab}{title for y axis, if NULL value depends on bplottype} 
 \item{\dots}{other arguments to the \code{plot} function can be passed here.}
}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "ANY", y = "ANY")}}{Generic function: see
      \code{\link[graphics]{plot}}.}
    \item{\code{signature(x = "boundEst", y = "missing")}}{Plot
      binary sequential boundaries for \code{x}.}
  }
}
\details{
The default rcol vector are good colors for distinguishing for those with color blindness. Text is printed on the unused portion of the plot, which uses the color names 
taken from the rcol vector  names. 

Their are several different types of plots, selected by the \code{bplottype} argument, where the value is a character string with 2 characters, the first representing the 
x-axis and the second representing the y-axis. For example \code{bplottype}='NS' denotes N=total number of trials on the horizontal axis, and S=number of successes on the vertical 
axis. Other plots are: 'FS'=failure by successes; 'NB'=total by B-values; 'NZ'=total by Z-scores; 'NE'=total by estimates and confidence intervals. The type 'NE' is only defined 
if there are only 1 value for each N on the upper and 1 value for each N on the lower part of the boundary. Otherwise, the confidence intervals would overlap and be uninformative. 
For 'NE' the end of the boundary is not plotted because of that overlapping.  

For some examples, see plot section of the vignette. 
}
\examples{
b<-designOBF(50,theta0=.3)
plot(b,bplottype="NE")
}
\keyword{methods}

