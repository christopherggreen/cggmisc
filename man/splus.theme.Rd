\name{S-Plus Graphics Theme}
\alias{splus.theme}
\alias{splus.colors}
\title{ S-Plus Graphics Color Themes } 
\description{
    \code{splus.colors} and \code{splus.theme} are
	used in lattice plots to replicate the default 
	color scheme used in S-Plus trellis graphics.
}
\usage{
  splus.colors(i=NULL)
  splus.theme(name = .Device, color = (name != "postscript") )
}
\arguments{
  \item{i}{Integer index into color table (defined below). Currently
    valid values are 1 to 17 inclusive. Default value, NULL, returns
	the entire data frame.}
  \item{name}{ The device to use for plotting, e.g., \code{windows}. 
    Defaults to the active graphics device (\code{.Device}). }
  \item{color}{ Use colors or grayscale? Defaults to TRUE for all 
    devices except the \code{postscript} device. }
}
\details{
	\code{splus.colors} defines the following colors:
    \tabular{rrl}{
      RGB Value \tab Index \tab Color Name \cr
      (  0,  0,  0) \tab  1 \tab black\cr
      (255,  0,  0) \tab  2 \tab red\cr
      (  0, 64,128) \tab  3 \tab navy\cr
      (  0,255,  0) \tab  4 \tab green\cr
      (255,127,  0) \tab  5 \tab orange\cr
      (  0,126,255) \tab  6 \tab sky blue\cr
      (255,255,  0) \tab  7 \tab yellow\cr
      (255,  0,  0) \tab  8 \tab red\cr
      (198,255,255) \tab  9 \tab pastel seagreen\cr
      (255,195,255) \tab 10 \tab pastel lavender\cr
      (200,255,200) \tab 11 \tab pastel light green\cr
      (255,209,143) \tab 12 \tab pastel light orange\cr
      (169,226,255) \tab 13 \tab pastel light blue\cr
      (255,255,195) \tab 14 \tab pastel light yellow\cr
      (255,140,138) \tab 15 \tab pastel dark pink\cr
      (110,110,100) \tab 16 \tab dark grey\cr
      (128,128,128) \tab 17 \tab darker grey\cr
	}

	\code{splus.theme} sets up a theme for use with 
	lattice graphics.
}
\value{
	\code{splus.colors} returns a data frame of RGB color
	values.

	\code{splus.theme} returns a list compatible with a
	lattice theme.
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} }
\seealso{\code{\link[lattice]{trellis.device}}}
\examples{
    # normal distribution example
    n.cases  <- 2
    n.trials <- 4
    n.sample <- 250
    # generate sample data
    normal.sample.data <- as.data.frame(
        list(
            ret=rnorm(n.trials*n.cases*n.sample, 
                mean=rep(seq(-5,5,length=n.trials*n.cases),each=n.sample), 
                sd=rep(seq(0.1,10,length=n.trials*n.cases),each=n.sample)),
            trial=rep(paste("TRIAL 0",1:n.trials,sep=""),each=n.cases*n.sample),
            case=rep(paste("CASE ",LETTERS[1:n.cases],sep=""),
                n.trials*n.sample)
        )
    )
	xxx <- bwplot( case~ret | trial, 
	    data=normal.sample.data )
	trellis.device(theme=splus.theme())
	print(xxx)
}
\keyword{ dplot }
