\name{cggRutils_is.zero}
\alias{is.zero}
\title{Test for a zero value}

\description{
  Tests whether the elements of a vector, matrix, or array 
  are equal to zero.
}

\usage{
is.zero(x)

\method{is.zero}{default}(x)
}

\arguments{

  \item{ x }{ \code{x} a vector, matrix, array, or other numeric
  	 variable. }
}


\details{
	The test performed for each element of \code{x} is 

		((not missing) and (equal to zero)).
}

\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\keyword{is.zero}
