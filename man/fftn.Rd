\name{fftn}
\alias{fftn}
\title{n-point FFT}
\description{
  Performs n-point Fast Fourier Transform (FFT) 
  analoguous to MATLAB's \code{fft(x,n)}.
}
\usage{
  fftn(x, n=length(x))
}
\arguments{
   \item{x}{vector or array on which to perform the FFT.}
   \item{n}{Desired length of the FFT. Defaults to length of \code{x}.}
}
\details{
  If \code{x} is a vector, \code{fftn} either truncates 
  \code{x} or pads it with trailing zeros to make its 
  length exactly \code{n}. For array, each column is adjusted
  in this way.

  After padding or truncating, the routine calls \code{fft} on
  the altered vector or array.
}
\value{
	Return value is the same as \code{fft}.
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\seealso{ \code{\link{ftt}} }
\examples{
   fftn(1:200,n=256)
}
\keyword{ }
