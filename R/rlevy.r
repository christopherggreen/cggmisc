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
"rlevy" <- function(n, location=0., scale=1., use.names=TRUE) {
   
   if ( any(scale < 0) ) stop("scale must be positive")
      
   n <- as.vector(n)
   
   nn <- length(n)
   nl <- length(location)
   ns <- length(scale)
   if ( use.names ) 
      dn <- list(NULL, as.character(location), as.character(scale))
   
   location <- aperm(array(rep(location,each=n*ns),c(n,ns,nl)),c(1,3,2))
   scale    <- array(rep(scale, each=n*nl),c(n,nl,ns))
   
   out <- array(rnorm(n*nl*ns),c(n,nl,ns))
   out <- 1./(out*out)
   out <- scale*out + location
   if ( use.names ) dimnames(out) <- dn
   drop(out)
}
