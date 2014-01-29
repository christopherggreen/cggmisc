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
"plevy" <- function(x, location=0., scale=1., use.names=TRUE) {
   
   if ( any(scale < 0) ) stop("scale must be positive")
      
   x <- as.vector(x)
   
   nx <- length(x)
   nl <- length(location)
   ns <- length(scale)
   if ( use.names ) 
      dn <- list(as.character(x), as.character(location), as.character(scale))
   
   x        <- array(rep(x,times=ns*nl),c(nx,nl,ns))
   location <- aperm(array(rep(location,each=nx*ns),c(nx,ns,nl)),c(1,3,2))
   scale    <- array(rep(scale, each=nx*nl),c(nx,nl,ns))
   
   z  <- x - location
   Fx <- array(rep(NA,length.out=length(z)),c(nx,nl,ns))
   Fx[z <= 0] <- 0
   inb <- z > 0
   Fx[inb] <- 2 * (1 - pnorm(sqrt(scale[inb]/z[inb])))
   if ( use.names ) dimnames(Fx) <- dn
   drop(Fx)
}
