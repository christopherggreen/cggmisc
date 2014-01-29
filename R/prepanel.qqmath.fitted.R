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
### Based on prepanel.default.qqmath from the lattice package written by 
### Deepayan Sarkar. 
###
### See the COPYRIGHTS file in the top-level package directory for a full 
### list of copyright and authorship information.
###
"prepanel.qqmath.fitted" <-
function (x, 
    f.value=NULL, 
    distribution = qnorm, 
    quantile.type = 7, 
    groups = NULL, 
    subscripts = NULL,
    distribution.fit=NULL, 
    count = 1, 
    verbose = FALSE, 
    storeframe=parent.frame(2), ...) 
{
    if (!is.numeric(x)) 
        x <- as.numeric(x)

    distribution <- if (is.function(distribution)) 
        distribution
    else if (is.character(distribution)) 
        get(distribution)
    else eval(distribution)

    # get distribution fitting function
    distribution.fit <- if (is.function(distribution.fit)) 
        distribution.fit
    else if (is.character(distribution.fit)) 
        get(distribution.fit)
    else eval(distribution.fit)

    getxx <- function(x, distribution, distribution.fit, 
        f.value = NULL, nobs = sum(!is.na(x))) {

        if ( length(x) ) {
            wna  <- which.na(x)

            # fit distributional arguments using distribution.fit
            fitted.args <- distribution.fit(if ( length(wna) ) x[-wna] else x)

            if ( verbose ) {
                cat("In prepanel.qqmath.fitted#getxx: fitted parameters are:\n")
                print(fitted.args)
            }

            # set up a call to distribution with x and the fitted arguments
            # use DATA as a placeholder for actual values
            distcall <- c(list(distribution,quote(DATA)), fitted.args)

            # compute x and y for panel function
            if ( is.null(f.value) ) {
                # f.value is null,  use ppoints(nobs) as the default
                # set first argument to distribution
                distcall[[2]] <- quote(ppoints(nobs))
            } else if ( is.numeric(f.value) ) {
                distcall[[2]] <- quote(f.value)
            } else {
                distcall[[2]] <- quote(f.value(nobs))
            }
            xx <- eval(as.call(distcall))
        } else {
            xx <- x
            fitted.args <- NULL
        }
        
        list(xx=xx, fitted.args=fitted.args)
    } # end getxx

    getyy <- function(x, quantile.type, f.value = NULL, nobs = sum(!is.na(x))) {

            # compute x and y for panel function
        if ( is.null(f.value) ) {
            sort(x)
        } else if ( is.numeric(f.value) ) {
            quantile(x, f.value, names = FALSE, 
                type = quantile.type, na.rm = TRUE)
        } else {
            quantile(x, f.value(nobs), names = FALSE, 
                type = quantile.type, na.rm = TRUE)
        }

    }


    wna  <- which.na(x)
    
    nobs <- length(x) - length(wna)

    output <- NULL

    # if all missing call prepanel.null()
    if ( nobs ) { 
    
        # do fitting here, then push result back upwards to qqmath.fitted
        # which is two frames up 
        # (qqmath.fitted --> limits.and.aspect --> prepanel)
        # this way the panel function does not
        # need to refit. Useful if the fitting function is expensive to call.

        # qqmath.fitted is two frames up
        fitted.args.global <- get("fitted.args", envir=storeframe )    
            
        if (!is.null(groups)) {
            sx <- split(x, groups[subscripts])
            xxlist <- lapply(sx, getxx, distribution = distribution, 
            distribution.fit=distribution.fit, f.value = f.value)
            yylist <- lapply(sx, getyy, quantile.type = quantile.type, 
                f.value = f.value)
            fitted.args.global[[count]] <- structure(lapply(xxlist, 
                function(z) z$fitted.args), names=names(sx))
            output <- list( xlim = range(unlist(lapply(xxlist, function(z) z$xx)), 
                finite = TRUE), ylim = range(unlist(yylist), finite = TRUE), 
                dx = unlist(lapply(xxlist, function(z) diff(z$xx))), 
                dy = unlist(lapply(yylist, diff))
            )
         } else {
            xx <- getxx(x, distribution = distribution, 
                distribution.fit = distribution.fit, f.value, nobs)
            yy <- getyy(x, quantile.type, f.value, nobs)

            fitted.args.global[[count]] <- xx$fitted.args

            # 7/16/2010 small bug here: should be scale.limits(xx$xx) not scale.limits(xx)
            output <- list(xlim = lattice:::scale.limits(xx$xx), 
                ylim = lattice:::scale.limits(yy), 
                dx = diff(xx$xx), dy = diff(yy))
        }

        assign("fitted.args", fitted.args.global, envir=storeframe)

    } else {
        output <- lattice:::prepanel.null()
    }

    output
}
