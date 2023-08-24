\name{designOBF}
\alias{designOBF}
\alias{designOBFpower}
\alias{designAb}
\alias{designFixed}
\alias{designFixedpower}
\alias{designSimon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Design Sequential Binary Boundary
}
\description{
There are several functions that create binary sequential boundaries. The function \code{designAb} allows great flexibility in creating 
user defined boundaries. The functions \code{designOBF} and \code{designOBFpower} create boudaries of the O-Brien-Fleming type, extending those 
boundaries to allow looks after every observation. The former (\code{designOBF}) uses a user defined maximum number of observations (Nmax), while 
the latter  (\code{designOBFpower}) uses the power argument to try to find a design with a smaller maximum that achieves the desired power. 
The functions \code{designFixed} and \code{designFixedpower} are analogous for fixed sample designs. The function \code{designSimon} uses the \code{\link[clinfun]{ph2simon}}
from the \pkg{clinfun} package to create boundaries using Simon's (1989) two-stage design.

}
\usage{

designAb(Nk, a = NULL, b = NULL, theta0 = NULL, 
    tsalpha = NULL, alternative = "two.sided", 
    conf.level = 0.95, binding = "both")

designOBF(Nmax,theta0 = 0.5,  k = Inf,  tsalpha = NULL,
    alternative = "two.sided", conf.level = 0.95, 
    binding = "both")
designOBFpower(theta0 = 0.5, theta1=.6,  k=Inf,
    power=.9, tsalpha = NULL, alternative = "two.sided",
    conf.level = 0.95, binding = "both", allNgreater=FALSE,
    checkmax=10, maxNmax=2*ss)

designFixed(Nmax, theta0 = 0.5, tsalpha = NULL, 
    alternative = "two.sided", conf.level = 0.95)
designFixedpower(theta0 = 0.5, theta1 = 0.6, power = 0.8,
    maxNmax = Inf, tsalpha = NULL, alternative = NULL,
    conf.level = 0.95, allNgreater = FALSE)

designSimon(theta0, theta1, alpha = 0.05, beta = 0.2, 
    type = c("optimal", "minimax"), nmax=100)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nk}{vector of unique N values where there is stopping}
  \item{a}{numeric vector with length(a)=length(Nk)-1, stop if number of successes out of Nk[i] is less than or equal to a[i] (see details)}
  \item{b}{numeric vector with length(a)=length(Nk)-1, stop if number of successes out of Nk[i] is greater than or equal to b[i] (see details)}
  \item{Nmax}{maximum number of observations for the design}
  \item{maxNmax}{maximum number for Nmax (see details)}
  \item{k}{number of looks at the data, Inf denotes looking after each observation}
  \item{theta0}{probability of success under the null}
  \item{tsalpha}{vector of length 2 with nominal significance levels for each side, if not NULL overrides \code{conf.level} and \code{alternative} 
       (see \code{\link{getTSalpha}})
   }
  \item{conf.level}{confidence level, ignored if tsalpha is not NULL}
  \item{alternative}{character, alternative hypothesis, either 'less', 'greater' or 'two.sided' }
  \item{binding}{character, which sides are binding: 'both', 'upper', or 'lower'}
\item{theta1}{probability of success under alternative for power calculations}
\item{power}{nominal power, boundary strives to have power under the alternative at least equal to power}
\item{allNgreater}{logical, if TRUE max(N) will be at least as large as the fixed sample size for which all greater N have power>power}
\item{checkmax}{integer, on the iteration  checkmax, check that Nmax has power at least power}
\item{alpha}{one sided alpha level for test theta>theta0}
\item{beta}{1-power, for theta1}
\item{type}{character, type of 2-stage design, either 'optimal' or 'minimax'}
\item{nmax}{maximum total sample size, cannot be higher than 1000}
}
\details{
The \code{tsalpha}, \code{alternative}, and \code{conf.level} are input into the \code{\link{getTSalpha}} function to output a \code{tsalpha} vector. The \code{tsalpha} 
vector allows the nominal error to be different on each side. For details see \code{\link{getTSalpha}}.

For \code{designAb}, when you do not want to stop on the lower or upper boundary at any value of Nk, the associated value of a (lower) or b (upper) should be NA. 

The \code{designOBF} function calculates a boundary that stops whenever the B-value (Lan and Wittes, 1988) is larger than one cutoff value or smaller than a different cutoff value. 
The cutoff values are chosen so that the probability of spending alpha on the appropriate side 
is almost all spent while still rejecting at at least one end value of the boundary. 

The function \code{designOBFpower} repeatedly calls \code{designOBF} and finds the design that 
gives sufficient power under a given alternative. Specifically, by setting  
 Nmax to Nmaxi in \code{designOBF}, where Nmaxi is increased by 1 at each iteration. The initial Nmaxi is either 
the first N that gives a large enough power in the fixed sample size design (allNgreater==FALSE) or the first N such that all larger N will give enough power for fixed samples (allNgreater==TRUE). 
On the \code{(checkmax)}th iteration, check that the power will be large enough when Nmaxi equals Nmax (from designOBFpower call). 
So if you set \code{checkmax}=1 then you will
check the largest value of Nmax first, but this may be inefficient since larger values of Nmax in the \code{obf} call are slower.  


See Kirk and Fay (2014) for an introductory paper about exact binary sequential tests using the binseqtest package.
}
\value{
a object of class \code{\link{boundEst}}
}
\references{
Kirk, J, and Fay, MP (2014). An Introduction to Practical Sequential Inferences via Single Arm Binary Response Studies Using the binseqtest R Package. (to appear in American Statistician).

Lan, KKG, and Wittes, J (1988). The B-Value: A Tool for Monitoring Data. Biometrics 44:579-585.

Simon R. (1989). Optimal Two-Stage Designs for Phase II Clinical Trials. Controlled Clinical Trials 10, 1-10. 
}
%\author{%%  ~~who you are~~}
%\note{%%  ~~further notes~~}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
