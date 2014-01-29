\name{levy}
\alias{levy}
\alias{Levy Distribution}
\alias{dlevy}
\alias{plevy}
\alias{qlevy}
\alias{rlevy}
\title{
   The Levy Distribution 
}
\description{
   Density, distribution function, quantile function, and random number
	generation for the Levy distribution a.k.a alpha-stable 
	distribution with index 1/2 and skewness 1.
}
\usage{
   dlevy(x, location=0, scale=1, use.names=TRUE)
	plevy(q, location=0, scale=1, use.names=TRUE)
	qlevy(p, location=0, scale=1, use.names=TRUE)
	rlevy(n, location=0, scale=1, use.names=TRUE)
}
\arguments{
   \item{x,q}{vector of quantiles}
	\item{p}{vector of  probabilities}
	\item{n}{number of samples to generate}
	\item{location}{the location parameter}
	\item{scale}{the scale parameter}
	\item{use.names}{Default is TRUE}
}
\details{
	The Levy distribution is a special case of the 
	alpha-stable distribution.

	The density for the Levy distribution has the form 
	\deqn{\sqrt(\sigma/(2*\pi)) * (x - \mu)^(-(3/2)) 
	* \exp(-(\sigma/(2*(x-\mu))))}{%
	\sqrt(\sigma/(2*\pi)) * (x - \mu)^(-(3/2)) 
	* \exp(-(\sigma/(2*(x-\mu))))}
	where \eqn{\mu}{\mu} is the location parameter and
	\eqn{\sigma}{\sigma} is the scale parameter.

	It is supported on \eqn{(\mu, \infty)}{(\mu, \infty)}. 
	The distribution for the Levy distribution with 
	location 0 has the form
	\deqn{F(x) = 2 * (1 - \Phi(\sqrt(\sigma/x)))}{%
	F(x) = 2 * (1 - \Phi(\sqrt(\sigma/x)))}.
			
	Quantiles are found by solving \eqn{F(x) = p}{F(x) = p}.

	Random numbers from the Levy distribution can be 
	generated via the transformation \eqn{\sigma Z^{-2} + \mu}{%
	\sigma Z^{-2} + \mu}, where \eqn{Z}{Z} is drawn from 
	the standard normal distribution.
}
\value{
	The Levy distribution will be evaluated at each point 
	in \code{x} (or \code{p} or \code{q}). The output 
	will be an array whose dimensions are the given by the 
	lengths of \code{x}, \code{location}, and \code{scale}.
	Any dimension with length 1 will be dropped.
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\references{
   Samorodnitsky, Gennady and Taqqu, Murad S. (1994) 
   \emph{Stable Non-Gaussian Random Processes},
	Chapman \& Hall / CRC, Boca Raton.
}
\seealso{ There is an implementation of the PDF, etc., of the 
	general alpha-stable distribution in the \code{stabledist} 
	package; see the \code{\link{dstable}}, \code{\link{pstable}},
	\code{\link{qstable}}, and \code{\link{rstable}} functions
	in that package.
}
\examples{
dlevy( seq(-5,5,0.1) )
plevy( seq(-5,5,0.1), location=3, scale=2 )
}
\keyword{distribution}
