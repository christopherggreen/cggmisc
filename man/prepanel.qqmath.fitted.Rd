\name{prepanel.qqmath.fitted}
\alias{prepanel.qqmath.fitted}
\title{ Prepanel function for qqmathFitted }
\description{
    Prepanel function for \code{qqmathFitted}
}
\usage{
    prepanel.qqmath.fitted(x, 
        f.value = NULL, 
        distribution = qnorm, 
        quantile.type = 7, 
        groups = NULL, 
        subscripts = NULL, 
        distribution.fit = NULL, 
        count = 1, 
        verbose = FALSE, 
        storeframe = parent.frame(2), ...)
}
\arguments{
  \item{x}{ vector of observations (passed in from calling lattice 
      function, e.g., \code{qqmathFitted} }
  \item{f.value}{ vector of percentiles at which to compute quantiles 
      from the reference distribution. By default, \code{ppoints} is used 
      to specify sensible percentiles. Override this option with care. }
  \item{distribution}{ function to compute quantiles from the reference 
      distribution. Can be a named function (e.g., \code{qnorm}), a character 
      string containing the name of a function (e.g., \code{"qnorm"}), or an 
      anonymous function.}
  \item{quantile.type}{ Numeric value, passed to \code{quantile}. Controls the 
      type of sample quantiles that will be taken from the observations \code{x}. 
      See the help page for \code{quantile} for more detail.  }
  \item{groups}{ Standard \code{lattice} argument defining data groups. See the 
      help page for \code{xyplot}.}
  \item{subscripts}{ Standard \code{lattice} argument for use with \code{groups}. 
      See the help page for \code{xyplot}.}
  \item{distribution.fit}{ function to fit the parameters of the reference 
      distribution (defined in \code{distribution}). Can be a named function, a 
      character string containing the name of a function, or an anonymous function. 
      The function should return a list of parameters to \code{distribution} with 
      names matching argument names to \code{distribution}.  }
  \item{count}{ This argument is for use by the calling function in conjunction 
      with \code{storeframe}. It is not intended for use by the end user. }
  \item{verbose}{ If TRUE, additional debugging information will be printed. }
  \item{storeframe}{ This is the frame in which the fitted distributional parameters 
      are stored to avoid potentially expensive recalculation. Do not modify this 
      argument unless you have read and understood the source code to 
      \code{qqmathFitted}. }
  \item{\dots}{ This is provided for future use. Additional arguments will
      be ignored silently at this time. }
} 
\details{
	 \code{prepanel.qqmath.fitted} is a modified version of 
	 \code{prepanel.default.qqmath} from Deepayan Sarkar's \code{lattice} package. 

    \code{prepanel.qqmath.fitted} does the actual distributional fitting for 
    \code{qqmathFitted}. The results of the fitting are stored in the frame
    used by \code{qqmathFitted} so that they do not need to be recomputed by
    \code{panel.qqmath.fitted}.
}
\value{ A list containing information on the limits of the plot. See
    \code{\link{prepanel.default.xyplot}} for more details on prepanel 
    functions.}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} for
	\code{prepanel.qqmath.fitted}. \code{prepanel.qqmath.fitted} is a modified
	version of \code{prepanel.default.qqmath} from the \code{lattice} package, 
	written by Deepayan Sarkar. 
}
\seealso{
    \code{\link{qqmathFitted}}, \code{\link{panel.qqmath.fitted}},
    \code{\link{qqmath}}, \code{\link{panel.qqmath}}, 
    \code{\link{prepanel.default.xyplot}}
}
\examples{
    # see qqmathFitted for examples
}
\keyword{ dplot }
