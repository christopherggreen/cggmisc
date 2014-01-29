\name{likelihood.functions.t}
\alias{likelihood.t}
\alias{score.t}
\alias{info.exp.t}
\alias{asy.var.t}
\title{
    Functions for Maximum Likelihood Estimation of 
    Student's T Distribution
}
\description{
    These functions are useful for fitting a location-scale
    model based on Student's T distribution via maximum
    likelihood.  
}
\usage{
likelihood.t(x, obs)
score.t(x, obs)
info.exp.t(x)
asy.var.t(x)
}
\arguments{
  \item{x}{ vector (of length 3) of parameters for the T
      distribution. The first entry is the degrees of freedom 
        parameter \eqn{\nu}{nu}; the second entry
        is the location parameter \eqn{\mu}{mu};
        and the third entry is the scale parameter
        \eqn{\sigma}{sigma}.
}
  \item{obs}{ a vector of observations. \code{obs} should not 
      include missing or non-finite values. }
}
\details{
    The distributional model in use here assumes that the
    random variable \eqn{X}{X} follows a location-scale
    model based on the Student's T distribution; that is,
    \deqn{\frac{X - \mu}{\sigma} \sim T_{\nu},}{%
        (X - mu)/(sigma) ~ T_{nu},}
    where \eqn{\mu}{mu} and \eqn{\sigma}{sigma} are location
    and scale parameters, respectively, and \eqn{\nu}{nu} is
    the degrees of freedom parameter of the T distribution.  
}
\value{
    \code{likelihood.t} returns the value of the likelihood
    function for the T distribution evaluated at the given
    parameter set \code{x} and observations \code{obs}.

    \code{score.t} returns a vector of length 3 containing
    the values of the partial derivatives of the likelihood
    function with respect to each of the parameters.

    \code{info.exp.t} returns the 3-by-3 expected
    information matrix evaluated at the given parameter
    set \code{x}.

    \code{asy.var.t} returns the 3-by-3 asymptotic variance
    matrix evaluated at the given parameter set \code{x}.
}
\references{
    The likelihood and score functions used were calculated 
    as part of one of the author's qualifying exams. See 
    Chapter 3 of the paper below.

    Green, C. G. (2005) Heavy-Tailed Distributions
    in Finance: An Empirical Study.  Qualifying Exam,
    Department of Statitics, University of Washington.
    Available from 
    \url{http://students.washington.edu/cggreen/uwstat/papers/computing_prelim_2005_green.pdf}

    More general calculations for the information matrix
    of general multivariate elliptic distributions can be
    found in the work of Ann F. S. Mitchell.

    Mitchell, Ann F. S. (1989) The Information Matrix, 
    Skewness Tensor and \eqn{\alpha}{alpha}-Connections 
    for the General Multivariate Elliptic Distribution.
    Annals of the Institute for Statistics and Math, 41 (2),
    pp 289-304.
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\seealso{
    \code{\link{fit.mle.t}}

    There are several other packages that offer functions to fit 
    the Student's t and related distributions using other approaches.
    \itemize{
	     \item{The \code{MASS} package contains the \code{fitdistr}
		  function, which can fit many univariate distributions via
		  maximum likelihood.}
        \item{The \code{PearsonDS} package fits general Pearson
        distributions, of which the Student's t distribution is
        a special case;}
        \item{The \code{GeneralizedHyperbolic} package fits the
        generalized hyperbolic distribution, of which the Student's
        t distribution is a special case.}
        \item{The \code{SkewHyperbolic} package fits the skew 
        hyperbolic distribution, of which the skewed t distribution
        is a special case.}
        \item{The \code{fGarch} package fits the Student's t 
        distribution via the \code{stdFit} function.}
        \item{The function \code{tFit} in the \code{fBasics}
        package fits the Student's t distribution.}
    }
}
\examples{
a <- -5 + 2*rt(1000, df=4)
# should be close to (0,0,0) indicating
# fit.mle.t found an extremum
score.t(unlist(fit.mle.t(a)),a)
}
\keyword{ distribution }
\keyword{ models }
\keyword{ Student's T }
