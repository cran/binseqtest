\name{stopTable}
\alias{stopTable}
\alias{print.stopTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create data frame with statistics for stopping boundary
}
\description{
Takes boundEst object and creates data frame with p-values and confidence intervals for stopping points between \code{Nmin} and \code{Nmax}.
}
\usage{
stopTable(object, Nrange = c(0,Inf), Srange=c(0,Inf),output="all",file="stopTableOutput.csv")

\method{print}{stopTable}(x,digits=c(3,5),maxnprint=Inf,\dots)
}
\arguments{
  \item{object}{object of class \code{\link{boundEst}}}
  \item{Nrange}{numeric vector, range of total trials to output boundary points (see details)}
  \item{Srange}{numeric vector, range of successes  to output boundary points (see details)}
  \item{output}{character, type of output, 'all','csv', or 'print'}
  %\item{digits}{vector for digits, first element is for estimates and confidence intervals, second for p-values}
  \item{file}{character, name of file to output comma separated file}
 \item{x}{object of class 'stopTable'}
 \item{digits}{vector of length 1 or 2, first element is digits to print for estimates and confidence intervals (and p-values if length(digits)=1), 
second element is digits for p-values}
\item{maxnprint}{number of rows to print from the table}
\item{\dots}{additional arguments to be passed to print function, not needed} 
}
\details{
Create table with statistics at selected stopping points. The arguments \code{Nrange} and \code{Srange}
select which points to output. If \code{Nrange} and \code{Srange} is of length 2, then the function selects output within the ranges of the associated N and S values.
If \code{Nrange} and \code{Srange} is of length 1,  then the function selects only output with exactly the specified values. Note that because \code{Nrange} is the only argument 
that starts with 'N', then \code{stopTable(object,N=41)} and \code{stopTable(oject,Nrange=41)} are equivalent. 
 Output is a \code{stopTable} object (default), a data.frame (output='data.frame'), or a .csv file (output='csv'). The \code{print.stopTable}
function is an S3 print function for \code{stopTable} objects,  i.e., it determines the default printing to screen for those objects.

}
\value{
Either a \code{stopTable} object, a data.frame, or a .csv file.
The \code{stopTable} object is a list with elements
\item{conf.level}{confidence level=1-lowerError-upperError}
\item{lowerError}{c.i. error on lower side}
\item{upperError}{c.i. error on upper side}
\item{nullTheta}{probability of success under null hypothesis}
\item{binding}{character, which boundaries are binding: 'both', 'lower', or 'upper'}
\item{alternative}{character, alternative hypothesis either 'two.sided', 'less', or 'greater'} 
\item{table}{ a data.frame with variables, S,N, estimate, lower (confidence limit), upper (confidence limit), pL (lower one-sided p-value), pU (upper one-sided p-value),
pts (two-sided p-value), and UL (character giving part of the boundary: 'lower', 'upper', or 'end') }

The data.frame (or .csv file) returns the table element of the \code{stopTable} with the other elements added as variables (or columns). 
}
%\references{%% ~put references to the literature/web site here ~}
%\author{%%  ~~who you are~~}
%\note{%%  ~~further notes~~}
%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
