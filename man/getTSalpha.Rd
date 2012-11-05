\name{getTSalpha}
\alias{getTSalpha}
\alias{getAlternative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-sided alpha, alternative, and confidence level 
}
\description{
Two functions to find \code{tsalpha} and \code{alternative}. 
}
\usage{
getTSalpha(tsalpha = NULL, alternative = NULL, conf.level = NULL)
getAlternative(tsalpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tsalpha}{vector of length 2 with nominal significance levels for each side, if not NULL overrides \code{conf.level} and \code{alternative} (see details)}
  \item{conf.level}{confidence level, ignored if tsalpha is not NULL}
  \item{alternative}{character, alternative hypothesis, either 'less', 'greater' or 'two.sided' }
}
\details{
The \code{tsalpha} is a vector of length 2 giving the nominal error for each side 
of confidence intervals. The function \code{getTSalpha} creates a \code{tsalpha} vector, allowing its creation either directly (non-null input for the argument \code{tsalpha}
simply outputs that same argument), or through the \code{alternative} and \code{conf.level} arguments.   
The element \code{tsalpha[1]} is the nominal error on the lower side, so for example if \code{tsalpha=NULL}, \code{alternative='greater'}, and\code{conf.level=.95}, 
then \code{getTSalpha} outputs the vector \code{c(0.05,0)}. In other words, if on rejection you want  to conclude that \eqn{\theta>\theta_0}, then you want all the nominal 
error to be on the lower side. Similarly  \code{tsalpha[2]} is the nominal error on the upper side,  and  \code{tsalpha=NULL}, \code{alternative='less'}, and\code{conf.level=.95}, 
gives \code{c(0,0.05)}. If \code{tsalpha=NULL}, \code{alternative='greater'}, and \code{conf.level=.95}, then outputs the vector \code{c(0.025,0.025)}.
 You must supply either \code{tsalpha} or both \code{alternative} and \code{conf.level}. 
}
\value{
\code{getTSalpha} returns a tsalpha vector (see details), and \code{getAlternative} gives the character vector for the appropriate alternative. 
}
%\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
\examples{
getTSalpha(conf.level=.95,alternative="two.sided")
getAlternative(c(0,.025))
}
%\keyword{ ~kwd1 }