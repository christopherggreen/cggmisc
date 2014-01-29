\name{qqmathFitted}
\alias{qqmathFitted}
\title{ Q-Q Plot Using a Fitted Distribution }
\description{
    \code{qqmathFitted} is a modified version of \code{qqmath} that 
     draws a quantile-quantile plot of sample data against a
     fitted reference distribution. The fitting is done during
     the panel construction using a user-provided function to 
     estimate distributional parameters.
}
\usage{
     qqmathFitted(formula, 
          data = NULL, 
          allow.multiple = is.null(groups) || outer, 
          outer = !is.null(groups), 
          distribution = qnorm, 
          f.value = NULL, 
          auto.key = FALSE, 
          aspect = "fill", 
          panel = panel.qqmath.fitted, 
          prepanel = prepanel.qqmath.fitted, 
          scales = list(), 
          strip = TRUE, 
          groups = NULL, 
          xlab, 
          xlim, 
          ylab, 
          ylim, 
          drop.unused.levels = lattice.getOption("drop.unused.levels"), 
          distribution.fit = function(y) list(mean = 0, sd = 1), 
          quantile.type = 7, 
          qqstyle = NULL, 
          add.qqline = c("none", "lm", "lmRob"), 
          add.qqline.par = if (!is.null(groups)) 
              trellis.par.get("panel.superpose") else 
                  trellis.par.get("add.line"), 
          envelope = TRUE, 
          rdist = rnorm, 
          rdist.args = NULL, 
          sig.level = 0.95, 
          mc.samples = 100, 
          seed = .Random.seed, 
          envstyle = NULL, 
          id.n = 3, 
          labelargs = FALSE, 
          verbose = FALSE, 
          ..., 
          lattice.options = NULL, 
          default.scales = list(), 
          subscripts = !is.null(groups), 
          subset = TRUE)
}
\arguments{
  \item{}{Most of these arguments work just like they do for
      \code{qqmath}; see the help files for \code{\link{qqmath}}
      and \code{\link{xyplot}} for more detail.}
  \item{formula}{ A lattice formula of the form
      \code{ ~ x | g1 * g2 * \dots}, where \code{x}
      is a numeric variable, and \code{g1}, \code{g2},
      etc., are conditioning variables. Typically all
      of these variables will be contained in \code{data}. }
  \item{data}{ A data source ( e.g., a data frame ) in
      which to find the variables referenced in 
      \code{formula}. }
  \item{allow.multiple}{ See \code{\link{xyplot}}. }
  \item{outer}{ See \code{\link{xyplot}}. }
  \item{distribution}{ function to compute quantiles from the reference 
      distribution. Can be a named function (e.g., \code{qnorm}), a character 
      string containing the name of a function (e.g., \code{"qnorm"}), or an 
      anonymous function.}
  \item{f.value}{ Vector of probabilities at which
      to compute quantiles. By default, \code{ppoints}
      is used to pick sensible values, so you generally
      don't need to specify \code{f.value}. See
      \code{\link{qqmath}} and 
      \code{\link{prepanel.qqmath.fitted}} for more details.}
  \item{auto.key}{See \code{\link{xyplot}}. }
  \item{aspect}{See \code{\link{xyplot}}. }
  \item{panel}{ A function that will be called once for each
      panel of the plot to do the actual plotting. See 
      \code{\link{xyplot}} for details about panel functions 
      in general. The default panel function here is 
      \code{\link{panel.qqmath.fitted}}.}
  \item{prepanel}{ Prepanel functions set sensible x- and y-limits 
      for panels; see \code{\link{xyplot}} for more details about  
      prepanel functions in general. The default prepanel function
      for \code{qqmathFitted} is \code{prepanel.qqmath.fitted}; it
      does the actual distributional fitting calculations. It is not
      recommended that you change the default function unless you've 
      read and understood the code for \code{prepanel.qqmath.fitted}. }
  \item{scales}{See \code{\link{xyplot}}. }
  \item{strip}{See \code{\link{xyplot}}. }
  \item{groups}{See \code{\link{xyplot}}. }
  \item{xlab}{See \code{\link{xyplot}}. }
  \item{xlim}{See \code{\link{xyplot}}. }
  \item{ylab}{See \code{\link{xyplot}}. }
  \item{ylim}{See \code{\link{xyplot}}. }
  \item{drop.unused.levels}{See \code{\link{xyplot}}. }
  \item{distribution.fit}{ function to fit the parameters of the reference 
      distribution (defined in \code{distribution}). Can be a named function, a 
      character string containing the name of a function, or an anonymous function.

      The function should return a list of parameters to \code{distribution} with
      names matching argument names to \code{distribution}.  }
  \item{quantile.type}{ Numeric value, passed to \code{quantile}. Controls the 
      type of sample quantiles that will be taken from the observations \code{x}.
      See the help page for \code{quantile} for more detail. }
  \item{qqstyle}{ List of graphical parameters to use for plotting the 
      observed and reference quantiles. }
  \item{add.qqline}{ Controls whether a fitted line should be added
      to the plot. The default option, \code{"none"}, does not add
      a line. The \code{"lm"} option adds a linear regression line
      (regressing observations on quantiles from the reference 
      distribution) using \code{"lm"}. The \code{"lmRob"} option
      adds a robust regression line, and requires the \code{robust}
      library. }
  \item{add.qqline.par}{ Optional list of parameters controlling 
      the appearance of the fitted line. }
  \item{envelope}{ If TRUE, plot a confidence region for the Q-Q plot 
      via simulation. Envelopes are not supported when groups are used,
      as the resulting plot would be quite cluttered.}
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
      in each panel.}
  \item{verbose}{ If TRUE, additional debugging information will be printed. }
  \item{\dots}{ Further arguments, often graphical parameters, 
      passed to internal plotting functions and eventually on 
      to \code{\link{panel.superpose}}, 
      \code{\link{panel.xyplot}}, and \code{\link{gpar}}. }
  \item{lattice.options}{See \code{\link{xyplot}}. }
  \item{default.scales}{See \code{\link{xyplot}}. }
  \item{subscripts}{See \code{\link{xyplot}}. }
  \item{subset}{See \code{\link{xyplot}}. }
}
\details{
	 \code{qqmathFitted} is a modified version of \code{qqmath}
	 from Deepayan Sarkar's \code{lattice} package. It adds the ability to 
	 fit the reference distribution to the data during the plotting procedure,
	 as well as the ability to simulation envelopes. The latter ability is
	 based on code appearing in the function \code{lmfmResQQPlot} from the
	 \code{robust} package maintained by Kjell Konis.

    \code{prepanel.qqmath.fitted} does the actual distributional fitting for 
    \code{qqmathFitted}. 
}
\value{
    An object of class \code{"trellis"}.
}
\author{ Christopher G. Green \email{christopher.g.green@gmail.com} for 
	\code{qqmathFitted}. \code{qqmathFitted} is a modified
	version of \code{qqmath} from the \code{lattice} package, written by Deepayan 
	Sarkar. The plotting code for simulation envelopes is based on code appearing
	in the \code{lmfmResQQPlot} function from the \code{robust} package
	written by Kjell Konis et al. 
}
\seealso{
    \code{\link{prepanel.qqmath.fitted}}, \code{\link{panel.qqmath.fitted}},
    \code{\link{qqmath}}, \code{\link{panel.qqmath}}
}
\examples{
    # normal distribution example
    n.trials <- 4
    n.sample <- 250
    # generate sample data
    normal.sample.data <- as.data.frame(
        list(
            ret=rnorm(n.trials*n.sample, 
                mean=rep(seq(-5,5,length=n.trials),each=n.sample), 
                sd=rep(seq(0.1,10,length=n.trials),each=n.sample)),
            trial=rep(paste("TRIAL",
                c(paste("0",1:9,sep=""),
                    as.character(10:n.trials))),each=n.sample),
            case=rep(c("CASE A","CASE B"),
                each=n.trials*n.sample \%/\% 2)
        )
    )
    # normal q-q plot
    qqmathFitted( ~ret, 
        data=normal.sample.data, 
        subset=1:n.sample,
        panel = panel.qqmath.fitted,
        main = "Normally-distributed trial data",
        ylab = "Quantiles of sample data",
        xlab = "Quantiles of fitted normal distribution",
        aspect = 1,
        as.table=TRUE,
        layout=c(1,1),
        distribution=qnorm,
        distribution.fit=function(y) 
            list(mean=as.vector(median(y)), sd=mad(y)),
        rdist=rnorm,
        qqstyle=list(pch="+",col=2),
        envstyle=list(pch="o",col=3)
    )
    # normal q-q plot, one panel per trial
    qqmathFitted( ~ret|trial, 
        data=normal.sample.data,
        panel = panel.qqmath.fitted,
        main = "Normally-distributed trial data",
        ylab = "Quantiles of sample data",
        xlab = "Quantiles of fitted normal distribution",
        aspect = 1,
        as.table=TRUE,
        layout=c(2,2),
        distribution=qnorm,
        distribution.fit=function(y) 
            list(mean=median(y), sd=mad(y)),
        rdist=rnorm,
        scales = list(relation="free"),
        qqstyle=list(pch="+",col=2),
        envstyle=list(pch="o",col=3)
    )
    # normal q-q plot showing subsetting
    qqmathFitted( ~ret, 
        data=normal.sample.data,
        groups = trial,
        subset = (trial \%in\% c("TRIAL 01","TRIAL 05")),
        panel = panel.qqmath.fitted,
        main = "Normally-distributed trial data",
        ylab = "Quantiles of sample data",
        xlab = "Quantiles of fitted normal distribution",
        aspect = 1,
        as.table=TRUE,
        layout=c(1,1),
        distribution=qnorm,
        distribution.fit=function(y) 
            list(mean=median(y), sd=mad(y)),
        envelope=FALSE,
        add.qqline="lm",
        add.qqline.par=list(lty=c(2,rep(NA,3),3),
        col=c("green",rep("",3),"black"))
    )
    # student t example
    # generate sample data
    symt.sample.data <- as.data.frame(
        list(
            ret=rep(seq(-5,5,length=n.trials),each=n.sample) +
                1.2*rt(n.trials*n.sample, df=4),
            trial=rep(paste("TRIAL",
                c(paste("0",1:9,sep=""),
                    as.character(10:n.trials))),each=n.sample)
        )
    )
    # symmetric t q-q plot
    qqmathFitted( ~ret, 
        data=symt.sample.data, 
        subset=1:n.sample,
        panel = panel.qqmath.fitted,
        main = "Symmetric t-distributed trial data",
        ylab = "Quantiles of sample data",
        xlab = "Quantiles of fitted symmetric t distribution",
        aspect = 1,
        as.table=TRUE,
        layout=c(1,1),
        distribution=function(p,df,mu,sigma) 
            mu + sigma*qt(p,df),
        distribution.fit=function(y) fit.mle.t(y),
        rdist=function(n,df,mu,sigma) 
            mu + sigma*rt(n,df),
        qqstyle=list(pch="o",col=2),
        envstyle=list(pch="+",col=3),
        labelargs=c("df"),
        verbose = TRUE
    )
}
\keyword{ dplot }
