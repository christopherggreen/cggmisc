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
	Estimates the parameters of an \eqn{\alpha}-stable distribution
	using McCulloch's quantile-based estimator. This estimator is 
	fast but not as accurate as other approaches (e.g., maximum likelihood).
	It is useful as a starting value in more accurate (but more 
	computationally intensive) estimates.
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
\author{ Based on J. H. McCulloch's GAUSS implementation, which can be found on McCulloch's
  homepage: http://www.econ.ohio-state.edu/jhm/jhm.html .

  S-PLUS/C code and R port by Christopher G. Green 
  \email{christopher.g.green@gmail.com}.
}
\references{
 J. Huston McCulloch, 
 "Simple Consistent Estimators of Stable Distribution Parameters",
 Communications in Statistics, Simulation, 1986 (15:4), 1109-1136.
}
\examples{
  mccullochquantile( rnorm(1000), symm=TRUE )

  # test case from mcculloch's paper
  require(stabledist) # for rstable

  # test how well the estimate works for different sample sizes
  results <- sapply(c(10, 100, 1000, 10000), 
  	function(n) sapply(1:1000, 
  		function(i,n) abs(mccullochquantile(rstable(n,1.45,0),symm=TRUE)$alpha - 1.45), 
  			n=n))
  
  colMeans(results)
  apply(results, 2, sd)
  
  # an asymmetric case 
  results <- sapply(c(10, 100, 1000, 10000), 
  	function(n) sapply(1:1000, 
  		function(i,n) abs(mccullochquantile(rstable(n,1.45,0.2))$beta - 0.2), 
  			n=n))
  
  colMeans(results)
  apply(results, 2, sd)
}
\keyword{distribution}
