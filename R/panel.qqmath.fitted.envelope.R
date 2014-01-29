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
panel.qqmath.fitted.envelope <- function(x, 
    rdist, 
    rdist.args, 
    sig.level, 
    mc.samples,
    seed = .Random.seed, 
    envstyle = NULL, 
    verbose=FALSE, ...) 
{

    # which order statistics to use for envelope
    sample.ci <- function(z, mc.samples, sig.level)
    {
        lower <- round(sig.level * mc.samples) + 1
        upper <- round((1 - sig.level) * mc.samples)
        sort(z)[c(lower, upper)]
    }
    
    # calculate alpha / 2
    sig.level <- (1 - sig.level)/2
    
    n <- length(x)

   # get random number generating function
    rdist <- if (is.function(rdist)) 
        rdist
    else if (is.character(rdist)) 
        get(rdist)
    else eval(rdist)


    if ( verbose ) {
        # put this here to see fitted values while plotting
        cat("Fitted values are:",unlist(rdist.args),"\n")
    }
    
    # Modify rdist function based on rdist.args

    rngcall <- as.call(c(list(rdist,mc.samples * n), rdist.args))

    # compute envelope
    temp.seed <- .Random.seed
    set.seed(seed)
    envelope <- matrix(eval(rngcall), n, mc.samples)
    set.seed(temp.seed)
    envelope <- apply(envelope, 2, function(z) sort(z,na.last=T))
    envelope <- t(apply(envelope, 1, sample.ci, mc.samples = mc.samples, 
        sig.level = sig.level))

    if(nrow(envelope) > 1) {
        # we will add an envelope            
        x <- sort(x)
        do.call("panel.xyplot", c(list(x = x, y = sort(envelope[,1]), type="p"), 
            envstyle, list(...)))
        do.call("panel.xyplot", c(list(x = x, y = sort(envelope[,2]), type="p"), 
            envstyle, list(...)))
    }

    # return the fitted values for future use
    rdist.args

}
