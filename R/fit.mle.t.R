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
### See the COPYRIGHTS file in the top-level package directory for 
### a full list of copyright and authorship information.
###
"fit.mle.t" <-
#
#
function(obsvals, mean.exists=TRUE, na.rm=FALSE, verbose=FALSE) {
    
    likelifun <- function(x,obs) { likelihood.t(x,obs) }
    score     <- function(x,obs) { score.t(x,obs) }
    
    if ( na.rm ) { 
        if ( length(nas <- which.na(obsvals) ) ) {
            obsvals <- obsvals[-nas]
        }
    }
    
    # initial values for the optimization
    # estimate df parameter using method of moments, location parameter using the mean,
    # scale paramter using MAD
    initvals <- c(max(2 + 2/((var(obsvals)/(mad(obsvals)^2)-1)),0.5), mean(obsvals), mad(obsvals))
    # if we are assuming the first moment exists, make sure the df parameter is at least 1
    if ( mean.exists ) initvals[1] <- max(initvals[1],1+.Machine$double.eps)
    # if there are missing values in the initial values, stop.
    if ( length(which.na(initvals)) ) {
        cat("fit.mle.t: mean:",mean(obsvals),"var:",var(obsvals),"mad:",mad(obsvals),"\n")
        stop("Missing values in initial values.")
    }
    if ( verbose ) cat("Optimization starting from ", initvals, "\n")
    
    result <- nlminb(initvals, objective=likelifun, gradient=score,
        lower=c(as.numeric(mean.exists)+.Machine$double.eps,-Inf,.Machine$double.eps),
        control=list(x.tol=1.0e-10),
        obs=obsvals,
    )
    
    if ( length(grep(result$message,"SINGULAR")) || length(grep(result$message,"FALSE")) ) {
        print(result)
        stop("nlminb failed to converge")
    }
    
    list("df"=result$par[1],"mu"=result$par[2],"sigma"=result$par[3])
}
