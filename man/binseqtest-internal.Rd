\name{binseqtest-internal}
\alias{binseqtest-internal}
\alias{validAbparms}
\alias{validBound}
\alias{validBoundNBF}
\alias{validBoundEst}
\alias{abCalcK}
\alias{abtoBound}
\alias{ciCalc}
\alias{pCalc}
\alias{abBindBothCalcK}
%\alias{getAlternative}
%\alias{getTSalpha}
\alias{missNAbparms}
\title{Internal functions}
\description{Internal functions, not to be called by user}
\usage{

validAbparms(object)
validBound(object)
validBoundEst(object)
validBoundNBF(object)

abBindBothCalcK(object)
abtoBound(from)

pCalc(S,N,K,order,theta0=.5,alternative="two.sided",ponly=FALSE)
ciCalc(S,N,K,order,type="upper",alpha=0.025)

missNAbparms(ab,missN=NULL,\dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{object, usually a boundary of some class}
  \item{from}{an object of class abparms}
  \item{S}{vector of number of successes}
  \item{N}{vector of number of trials}
  \item{K}{vector of number of ways to reach each bounary point}
  \item{order}{vector of ordering of boundary points}
  \item{theta0}{null value of probability of success each binary random variable}
  \item{alternative}{character, either 'two.sided', 'less', or 'greater'}
  \item{ponly}{logical, should only the specific p-value type given by alternative be calculated}
\item{type}{character, type of one-sided confidence interval to calculate, either 'upper' or 'lower'}
\item{alpha}{numeric, amount of error to allow on the one side of the confidence interval}
%\item{stats}{character, which stats to calculate, either 'all' or 'pval'}
%\item{conf.level}{numeric, level of confidence interval}
%\item{tsalpha}{numeric, two-sided alpha}
%\item{TSalpha}{numeric, two-sided alpha}
\item{ab}{object of class 'abparms'}
\item{missN}{numeric vector, the N values where  assessments are missed}
\item{\dots}{arguments passed to other functions, not used}
}
\details{
The validXX functions check that the object is a valid member of the class XX. For example, validBound checks that a bound object 
is OK by sum the probability distribution using the N,S, and K values and checking that it is within computer error of 1.  
The validity checks are run automatically by the new() function as part of the S4 implementation.

The function \code{abBindBothCalcK} takes an abparms object and creates a bound object. It requires calculating K, which is the number of ways 
to reach each boundary point. It ignores the \code{binding} argument and assumes all boundaries are binding. The \code{abtoBound} function
uses the \code{binding} argument to create either a \code{bound} object (for \code{binding}='both') or a \code{boundNBF} object otherwise. 
Users can use the \code{as} function to coerce an \code{abparms} object to a \code{bound} object.  

The function \code{pCalc} takes a boundary and calculates p-values, and outputs a vector of p-values (ponly=TRUE) or  list of 3 vectors (plower,pupper, pval). 

The function \code{cCalc} takes a boundary and calculates one of the one sided confidence intervals as directed by the type argument (either 'upper' or 'lower'). 

The functions \code{analyzeBound} and \code{analyzeBoundNBF} take objects of the 
\code{bound} and \code{boundNBF} classes and create ones of the \code{boundEst} and
\code{boundNBFEst} classes. This means basically that the confidence intervals and p-values 
are calculated that go with those bounds. 


The functions \code{getAlternative} and \code{getTSalpha} get those parameters from the inputs. 

The function \code{missNAbparms} modifies abparms objects to reflect missing assessments. This is the working function for the missN option in \code{modify}. 

}
%\value{}
%\references{}
%\author{}
%\note{}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
