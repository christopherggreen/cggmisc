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
### Deepayan Sarkar. 
###
### See the COPYRIGHTS file in the top-level package directory for a 
### full list of copyright and authorship information.
###
panel.qqmath.fitted.main <- function(x, 
  f.value=NULL, 
  distribution = qnorm, 
  quantile.type = 7, 
  groups = NULL, 
  subscripts = NULL, 
  fitted.args=NULL, 
  qqstyle=NULL, 
  verbose=FALSE, 
  group.number=NULL,
  add.qqline=c("none","lm","lmRob"),  
  add.qqline.par,
  ...) 
{

    # convert x to numeric
    x <- as.numeric(x)

    # get distribution function
    distribution <- if (is.function(distribution)) 
        distribution
    else if (is.character(distribution)) 
        get(distribution)
    else eval(distribution)

    if ( is.null(fitted.args) )
        stop("fitted.args is null in panel.qqmath.main.")

    wna  <- which.na(x)
    
    nobs <- length(x) - length(wna)

    # if all missing or x has length 0 do nothing
    if ( nobs ) { 

         if ( verbose ) {
             cat("In panel.qqmath.fitted: Fitted parameters are:\n")
             print(fitted.args)
         }

         if (!is.null(groups)) {
             # panel.superpose will recall this function for each group
             panel.superpose(x, y = NULL, f.value = f.value, 
                 distribution = distribution, quantile.type = quantile.type, 
                 groups = groups, subscripts = subscripts, 
                 panel.groups = panel.qqmath.fitted.main, qqstyle=qqstyle, 
                 fitted.args=fitted.args, add.qqline=add.qqline, 
                 add.qqline.par=add.qqline.par,
                 verbose = verbose, ...)
             # cgg: 7/9/2010 for now we're just returning x and y
             # won't be able to use those values in panel.qqmath.fitted
             # to draw envelopes, etc., but it's not clear what the
             # ``correct'' behavior of envelope/labelargs/etc.
             # should be when there are groups. 
             xx <- x  
             yy <- NULL
             fitted.args <- fitted.args
         } else {

             # set up a call to distribution with x and the fitted arguments
             # use DATA as a placeholder for actual values
             # the group number argument makes panel.superpose work above:
             # panel.superpose will pass group.number to each call of panel.groups
             # use default value of 1 when we are not in a groups situation

             # 7/16/2010 this does not work correctly in the non-groups 
             # situation---end up only using the first parameter of fitted.args
             # changed so that group.number is null by default, and we only use it
             # when it's non-null

             distcall <- c(list(distribution,quote(DATA)), 
                if ( is.null(group.number) ) fitted.args else 
                    fitted.args[[group.number]])
             # compute x and y for panel function
             if ( is.null(f.value) ) {
                 # f.value is null,  use ppoints(nobs) as the default
                 # set first argument to distribution
                 distcall[[2]] <- quote(ppoints(nobs))
                 yy <- sort(x)
             } else if ( is.numeric(f.value) ) {
                 distcall[[2]] <- quote(f.value)
                 yy <- quantile(x, f.value, names = FALSE, 
                     type = quantile.type, na.rm = TRUE)
             } else {
                 distcall[[2]] <- quote(f.value(nobs))
                 yy <- quantile(x, f.value(nobs), names = FALSE, 
                 type = quantile.type, na.rm = TRUE)
             }
             xx <- eval(as.call(distcall))

             # now call panel.xyplot
             do.call("panel.xyplot", c(list(x = xx, y = yy), qqstyle, list(...)))

             # adding lines
             add.qqline <- match.arg(add.qqline)

             if ( add.qqline != "none" ) {
                 # 7/16/2010 handle the grouped case
                 # nb user needs to pass in an add.qqline.par
                 # parameter that can be indexed by group.number
                 # properly: consider the effects of subset
                 if ( !is.null(group.number) ) {
                     add.qqline.par <- Rows(add.qqline.par, group.number)
                 }
                 # 7/16/2010 force evaluation of add.qqline.par
                 abline.call <- c(list(panel.abline,NA),add.qqline.par)
                 if ( add.qqline == "lm" ) {
                     abline.call[[2]] <- coef(lm(yy ~ xx))
                 } else if ( add.qqline == "lmRob" ) {
                     require(robust,quietly=TRUE)
                     abline.call[[2]] <- coef(robust::lmRob(yy ~ xx))
                 } else if ( add.qqline == "lmrob" ) { # can't get here yet
                     stop("lmrob option not available yet")
                     # require(robustbase,quietly=TRUE)
                 } 
                 eval(as.call(abline.call))
             }
         } # end null groups block
    } else {
         warning("All input values are missing---exiting.")
         xx <- yy <- fitted.args <- NULL
    }

    # return fitted.args for future use
    list(x=xx,y=yy,fitted.args=fitted.args)
}
