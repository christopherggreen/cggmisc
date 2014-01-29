### Copyright (c) 2012 Christopher G. Green <christopher.g.green@gmail.com>
###
### This file is part of the cggmisc package for R.
###
### This program is free software; you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation; either version 2 of the License, or
### (at your option) any later version.
###
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
###
### Based on panel.qqmath from the lattice package written by 
### Deepayan Sarkar and the lmfmResQQPlot function from the robust
### package maintained by Kjell Konis. 
### 
### See the COPYRIGHTS file in the top-level package directory for 
### a full list of copyright and authorship information.
###
panel.qqmath.fitted <- function(x, 
    f.value=NULL, 
    distribution = qnorm, 
    quantile.type = 7, 
    groups = NULL, 
    subscripts=NULL, 
    fitted.args=NULL,
    qqstyle=NULL, 
    envelope=TRUE, 
    rdist = rnorm, 
    rdist.args=NULL, 
    sig.level = 0.95, 
    mc.samples=100, 
    seed = .Random.seed, 
    envstyle=NULL,
    add.qqline = c("none","lm","lmRob"), 
    add.qqline.par, 
    id.n=3, 
    labelargs=FALSE, 
    verbose=FALSE, ...) 
{

    x <- as.numeric(x)
    n <- length(x)
    wna <- which.na(x)
    if ( (n==0) || (length(wna) == n) ) return()

    if ( is.null(fitted.args) ) {
        print(x)
        stop("fitted.args is null in panel.qqmath.fitted---debug.")
    }

    # plot quantiles of observations against those of fitted distribution
    panel.output <- panel.qqmath.fitted.main(x, f.value=f.value, 
        distribution=distribution, 
        quantile.type=quantile.type, groups=groups, 
        subscripts=subscripts,
        fitted.args=fitted.args, qqstyle=qqstyle, 
        verbose=verbose, 
        add.qqline=add.qqline, 
        add.qqline.par=add.qqline.par, ...)

    #fitted.args <- panel.output$fitted.args
    xx          <- panel.output$x
    yy          <- panel.output$y
    n <- length(xx)

    if ( is.null(rdist.args) )
        rdist.args <- fitted.args

    if ( !is.null(groups) )
    {
        # not supported at this time
        envelope <- FALSE
        id.n <- 0
        labelargs <- FALSE
    }

    # adding envelopes
    if ( envelope ) {
        panel.qqmath.fitted.envelope(xx,rdist=rdist,rdist.args=rdist.args,
            sig.level=sig.level,mc.samples=mc.samples,seed=seed,
            envstyle=envstyle,verbose=verbose,...)
    }
    
    # identify outliers
    if ( id.n ) {
        id.n <- order(abs(yy))[(n - id.n + 1):n]
        labels <- paste("  ", as.character((1:n)[id.n]), sep = "")
        ltext(xx[id.n], yy[id.n], labels, adj = 0)
    }        

    # add fitted arguments to the plot
    if ( is.character(labelargs) || (is.logical(labelargs) && labelargs) ) {
        
        if ( is.logical(labelargs) ) 
            labelargs <- names(rdist.args)
        args.to.label <- match(labelargs, names(rdist.args), nomatch=NA)
        if ( length(wna <- which.na(args.to.label) ) ) {
            warning("Some of the desired arguments could not found among the names of the fitted arguments.")
            args.to.label <- args.to.label[-wna]
        }
        labeltext <- paste(names(rdist.args)[args.to.label],
            sapply(rdist.args[args.to.label],function(x)round(x,2)),collapse=" ")

        add.text <- trellis.par.get("add.text")
        grid::grid.text(label = labeltext,
            x = grid::unit(0,"npc") + grid::unit(1, "char"),
            y = grid::unit(1,"npc") - grid::unit(1, "char"),
            just = 0,
            rot = 0,
            gp = grid::gpar(col = add.text$col, alpha = add.text$alpha,
                lineheight = add.text$lineheight,
                fontfamily = add.text$fontfamily,
                fontface = lattice:::chooseFace(add.text$fontface, add.text$font),
                cex = add.text$cex, ...)
        )
    }
} # end mypanel
