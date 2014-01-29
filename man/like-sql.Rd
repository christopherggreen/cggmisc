\name{\%like\%}
\alias{\%like\%}
\title{like operator for R}
\description{
   Provides a `like' operator for R similar to the `like' 
	operator in SQL for pattern matching
}
\usage{
	x %like% pattern
}
\arguments{
   \item{x}{A vector of character strings}
   \item{pattern}{A character string containing a search pattern}
}
\details{
 	underlying function is grep	
}
\value{
	A
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\seealso{
	\code{\link{grep}}	
}
\examples{
   data(state)
   subset( state.x77, row.names(state.x77) %like% "New" )
}
\keyword{ classes & character }
