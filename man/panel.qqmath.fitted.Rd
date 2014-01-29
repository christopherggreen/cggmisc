\name{panel.qqmath.fitted}
\alias{panel.qqmath.fitted}
\title{
    Panel function for \code{qqmathFitted}
}
\description{
    This is the default panel function for \code{qqmathFitted}.
}
\usage{
    panel.qqmath.fitted(x, 
        f.value = NULL, 
        distribution = qnorm, 
        quantile.type = 7, 
        groups = NULL, 
        subscripts = NULL, 
        fitted.args = NULL, 
        qqstyle = NULL, 
        envelope = TRUE, 
        rdist = rnorm, 
        rdist.args = NULL, 
        sig.level = 0.95, 
        mc.samples = 100, 
        seed = .Random.seed, 
        envstyle = NULL, 
        add.qqline = c("none", "lm", "lmRob"), 
        add.qqline.par, 
        id.n = 3, 
        labelargs = FALSE, 
        verbose = FALSE, ...)
}
\arguments{
  \item{x}{ vector of numeric data to be plotted in this panel }
  \item{f.value}{ Vector of probabilities at which
      to compute quantiles. By default, \code{ppoints}
      is used to pick sensible values, so you generally
      don't need to specify \code{f.value}. See
      \code{\link{qqmath}} and 
      \code{\link{prepanel.qqmath.fitted}} for more details.}
  \item{distribution}{ function to compute quantiles from the reference 
      distribution. Can be a named function (e.g., \code{qnorm}), a character 
      string containing the name of a function (e.g., \code{"qnorm"}), or an 
      anonymous function.}
  \item{quantile.type}{ Numeric value, passed to \code{quantile}. 
      Controls the type of sample quantiles that will be taken from 
        the observations \code{x}.  See the help page for \code{quantile} 
        for more detail.}
  \item{groups}{ Standard \code{lattice} argument defining data groups. 
      See the help page for \code{xyplot}.}
  \item{subscripts}{ Standard \code{lattice} argument for use with 
      \code{groups}.  See the help page for \code{xyplot}.}
  \item{fitted.args}{ This is a list of fitted parameters for the 
      reference distribution. The user should not specify these directly.
		Use the \code{distribution.fit} argument to 
		\code{\link{qqmathFitted}} to provide a function for estimating
		distributional parameters. }
  \item{qqstyle}{ List of graphical parameters to use for plotting the 
      observed and reference quantiles. }
  \item{envelope}{ If TRUE, plot a confidence region for the Q-Q plot 
      via simulation.}
  \item{rdist}{ The distribution from which to draw random numbers 
      for the confidence region simulation. It should correspond to 
      the distribution assumed in \code{distribution} and 
      \code{distribution.fit}. Just like \code{distribution}, 
      \code{rdist} can be specified as a named function, a character
      string containing the name of a function, or an anonymous
      function. }
  \item{rdist.args}{ A list containing additional arguments to the
      function specified in \code{rdist} (other than the parameters
      return by \code{distribution.fit}). }
  \item{sig.level}{ Significance level to use for the confidence region.
      Default is a 95\% condifence interval.}
  \item{mc.samples}{ Number of random samples to use to construct the 
      confidence region. }
  \item{seed}{ Seed to use for the random number generator. If specified,
      \code{.Random.seed} will be set to \code{seed} prior to generation of
      the simulation envelopes. This is useful to do for reproducibility of
      results. }
  \item{envstyle}{ List of graphical parameters to use for plotting the 
      confidence region boundaries. }
  \item{add.qqline}{ Controls whether a fitted line should be added
      to the plot. The default option, \code{"none"}, does not add
      a line. The \code{"lm"} option adds a linear regression line
      (regressing observations on quantiles from the reference 
      distribution) using \code{"lm"}. The \code{"lmRob"} option
      adds a robust regression line, and requires the \code{robust}
      library. }
  \item{add.qqline.par}{ Optional list of parameters controlling 
      the appearance of the fitted line. }
  \item{id.n}{ Number of outliers to label in each panel. Default is 3.
      Use \code{id.n = 0} or \code{id.n = FALSE} to omit labels for
		outliers. The outliers that will be flagged are the 
		\code{id.n} largest in absolute value. The labels correspond to
		the index of each point in the provided vector of observations.}
  \item{labelargs}{ Controls whether the fitted distributional parameters
      are added to each panel. Can be a logical value or a character value.
      If FALSE, no labels are added. If TRUE, labels are taken from the 
      names of \code{rdist.args}. If a character vector is provided, those
      strings are used to subset the names of \code{rdist.args}. The panel
      function \code{panel.qqmath.fitted} will prepend each label to the
      corresponding distributional parameter estimate, rounded to two 
      decimal places.

      \code{labelargs} is not supported when groups are used due to the 
      difficulty of automatically producing readable labels for all groups
      in each panel.
		
		The format of the labels is controlled by the lattice setting 
		\code{add.text}, and can be influenced by changing that setting. 
		Additional settings can be changed by passing arguments to \code{gpar}
		from the \code{grid} package via the \code{...} mechanism.
		}
  \item{verbose}{ If TRUE, additional debugging information will be printed. }
  \item{\dots}{ Further arguments, often graphical parameters, 
      passed to internal plotting functions and eventually on 
      to \code{\link{panel.superpose}}, 
      \code{\link{panel.xyplot}}, and \code{\link{gpar}}. }
}
\details{
	 \code{qqmathFitted} is a modified version of \code{qqmath}
	 from Deepayan Sarkar's \code{lattice} package. It adds the ability to 
	 fit the reference distribution to the data during the plotting procedure,
	 as well as the ability to simulation envelopes. The latter ability is
	 based on code appearing in the function \code{lmfmResQQPlot} from the
	 \code{robust} package maintained by Kjell Konis.

    \code{prepanel.qqmath.fitted} does the actual distributional fitting for 
    \code{qqmathFitted}. The results of the fitting are stored in the frame
    used by \code{qqmathFitted} so that they do not need to be recomputed by
    \code{panel.qqmath.fitted}.
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} for
	\code{panel.qqmath.fitted}. \code{panel.qqmath.fitted} is a modified
	version of \code{panel.default.qqmath} from the \code{lattice} package, 
	written by Deepayan Sarkar. The plotting code for simulation envelopes 
	is based on code appearing in the \code{lmfmResQQPlot} function from 
	the \code{robust} package written by Kjell Konis et al. 
}
\seealso{
    \code{\link{qqmathFitted}}, \code{\link{prepanel.qqmath.fitted}},
    \code{\link{qqmath}}, \code{\link{panel.qqmath}}
}
\examples{
    # see the examples for qqmathFitted
}
\keyword{ dplot }
