### Copyright (c) 2005-2012 Christopher G. Green <christopher.g.green@gmail.com>
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
"qlevy" <- function(p, location=0., scale=1., use.names=TRUE) {
   
   if ( any(scale < 0) ) stop("scale must be positive")
      
   p <- as.vector(p)

   if ( any( p < 0 | 1 < p ) ) 
     stop("Some input values were outside of [0, 1]")
   
   nx <- length(p)
   nl <- length(location)
   ns <- length(scale)
   if ( use.names ) 
      dn <- list(as.character(p), as.character(location), as.character(scale))
   
   p        <- array(rep(p,times=ns*nl),c(nx,nl,ns))
   location <- aperm(array(rep(location,each=nx*ns),c(nx,ns,nl)),c(1,3,2))
   scale    <- array(rep(scale, each=nx*nl),c(nx,nl,ns))
   
   # compute the quantiles for the standardized distribution
   qv <- array(qnorm(1.0 - 0.5 * as.vector(p)),c(nx,nl,ns))
   qv <- location + (scale / (qv*qv))
   if ( use.names ) dimnames(qv) <- dn
   drop(qv)
}
