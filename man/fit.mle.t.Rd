\name{fit.mle.t}
\alias{fit.mle.t}
\title{ Fit a T Distribution via Maximum Likelihood Estimation }
\description{
    Fits a location-scale model based on Student's \eqn{t}{t}
    distribution using maximum likelihood estimation.  
}
\usage{
fit.mle.t(obsvals, mean.exists = TRUE, na.rm = FALSE, verbose = FALSE)
}
\arguments{
  \item{obsvals}{ A vector of observations. }
  \item{mean.exists}{ Should it be assumed that the underlying
      T distribution has a finite first moment (i.e., that 
        \eqn{\nu > 1}{nu > 1})? By default it is assumed that 
        \eqn{\nu > 1}{nu > 1}, and this condition is enforced 
        in the fitting process.
}
  \item{na.rm}{ If TRUE, missing values in \code{obsvals} are 
      removed prior to fitting the distribution. Note: the fitting 
        process will likely fail if your data contains missing values 
        and you use \code{na.rm = FALSE}.
}
  \item{verbose}{
      If TRUE, diagnostics are printed during the fitting process. 
        Useful for debugging.
}
}
\details{
    \code{fit.mle.t} fits a location-scale model based
    on Student's \eqn{t}{t} distribution using maximum likelihood
    estimation.  The distributional model in use here
    assumes that the random variable \eqn{X}{X} follows
    a location-scale model based on the Student's \eqn{t}{t}
    distribution; that is, 
    \deqn{\frac{X - \mu}{\sigma} \sim T_{\nu},}{%
   (X - mu)/(sigma) ~ T_{nu},} 
    where \eqn{\mu}{mu} and \eqn{\sigma}{sigma} are location 
    and scale parameters, respectively, and \eqn{\nu}{nu} is 
    the degrees of freedom parameter of the T distribution.  
    \code{fit.mle.t} uses the likelihood and score functions 
    provided by \code{likelihood.t} and \code{score.t}.  
}
\value{
    \item{df}{Estimated degrees of freedom parameter \eqn{\nu}{nu}}
    \item{mu}{Estimated location parameter \eqn{\mu}{mu}}
    \item{sigma}{Estimated scale parameter \eqn{\sigma}{sigma}}
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
    \code{\link{likelihood.t}}, \code{\link{score.t}}

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
fit.mle.t(a)
}
\keyword{ distribution }
\keyword{ models }
\keyword{ Student's t }
