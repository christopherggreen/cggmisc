\name{cggmisc.mccullochquantile}
\alias{mccullochquantile}
\title{McCulloch's Quantile Estimator of the Parameters of an Alpha-Stable Distribution}
\description{
	Estimates the parameters of an \eqn{\alpha}-stable distribution using
	the quantile estimator of McCulloch (1986).
}
\usage{
	mccullochquantile(x, symm=FALSE)
}
\arguments{
  \item{ x }{ A numeric vector of data. Missing values (NA) are allowed, but will
    be removed during the calculation. } 
  \item{ symm }{Logical, TRUE to assume a symmetric \eqn{\alpha}-stable
    distribution. Default is FALSE. }
}
\details{

}
\value{ A vector of length 5 containing the parameter
 estimates (in this order):
 \tabular{ll}{
 alpha\tab the index of stability\cr
 beta\tab  the skewness parameter\cr
 c\tab     the scale parameter\cr
 delta\tab the location parameter\cr
 zeta\tab  the shifted location parameter (of Zolotarev)\cr
 }
}
\author{ Based on J. McCulloch's GAUSS implementation, which can be found on McCulloch's
  homepage: http://www.econ.ohio-state.edu/jhm/jhm.html .

  Ported to S-PLUS/C and later, to R, by Christopher G. Green 
  \email{christopher.g.green@gmail.com}.
}
\references{
 J. Huston McCulloch, 
 "Simple Consistent Estimators of Stable Distribution Parameters",
 Communications in Statistics, Simulation, 1986 (15:4), 1109-1136.
}
\examples{

}
\keyword{alpha-stable}
