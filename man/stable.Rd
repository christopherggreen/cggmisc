\name{stable}
\alias{fit.mle.stable}
\alias{likelihood.stable}
\title{ Fitting Stable Distributions } 
\description{
    Fit \eqn{alpha}-stable distributions using 
	maximum likelihood estimation.  
}
\usage{
fit.mle.stable(obsvals, method = c("nlminb","optim"),
  mean.exists=TRUE, robust.location=FALSE, strace=FALSE)
likelihood.stable(x, y, obs, mean.exists=TRUE) 
}
\arguments{
  \item{obsvals}{ A vector of observations. }
  \item{method}{Which optimization routine should be used to compute
    the MLE? Current options are \code{nlminb} and \code{optim}.
	Default is \code{nlminb}.}
  \item{mean.exists}{ Should it be assumed that the underlying
      stable distribution has a finite first moment (i.e., that 
        \eqn{\alpha > 1}{alpha > 1})? By default it is assumed that 
        \eqn{\alpha > 1}{alpha > 1}, and this condition is enforced 
        in the fitting process.
  }
  \item{robust.location}{Should a robust location estimate be used
    to compute the initial estimate of the MLE? Default is FALSE.}
  \item{strace}{}
  %\item{na.rm}{ If TRUE, missing values in \code{obsvals} are 
  %   removed prior to fitting the distribution. Note: the fitting 
  %     process will likely fail if your data contains missing values 
  %     and you use \code{na.rm = FALSE}.
  %
  \item{x}{}
  \item{y}{}
  \item{obs}{}
}
\details{
    \code{fit.mle.stable} fits an \eqn{\alpha}-stable model 
    using maximum likelihood estimation.  The distributional 
	model in use here assumes that the random variable 
	\eqn{X}{X} follows a distribution of the form
    \deqn{X \sim }
    %where \eqn{\mu}{mu} and \eqn{\sigma}{sigma} are location 
    %and scale parameters, respectively, 
	\eqn{\alpha}{alpha} is the tail index parameter of the stable distribution.  
    \code{fit.mle.stable} uses the likelihood function
    provided by \code{likelihood.stable}.
}
\value{
    \item{alpha}{}
    \item{beta}{}
    \item{sigma}{}
    \item{mu}{}
    \item{hessian}{}
}
\references{
    Green, C. G. (2005) Heavy-Tailed Distributions
    in Finance: An Empirical Study.  Qualifying Exam,
    Department of Statitics, University of Washington.
    Available from 
    \url{http://students.washington.edu/cggreen/uwstat/papers/computing_prelim_2005_green.pdf}

}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\seealso{
    \code{\link{fit.mle.t}}

    %There are several other packages that offer functions to fit 
    %the Student's t and related distributions using other approaches.
    %\itemize{
	%    \item{The \code{MASS} package contains the \code{fitdistr}
	%	 function, which can fit many univariate distributions via
	%	 maximum likelihood.}
    %   \item{The \code{PearsonDS} package fits general Pearson
    %   distributions, of which the Student's t distribution is
    %   a special case;}
    %   \item{The \code{GeneralizedHyperbolic} package fits the
    %   generalized hyperbolic distribution, of which the Student's
    %   t distribution is a special case.}
    %   \item{The \code{SkewHyperbolic} package fits the skew 
    %   hyperbolic distribution, of which the skewed t distribution
    %   is a special case.}
    %   \item{The \code{fGarch} package fits the Student's t 
    %   distribution via the \code{stdFit} function.}
    %   \item{The function \code{tFit} in the \code{fBasics}
    %   package fits the Student's t distribution.}
    %}
}
\examples{
}
\keyword{ distribution }
