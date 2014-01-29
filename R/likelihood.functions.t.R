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

"likelihood.t" <-
function(x, obs) { 
    -sum(log(dt((obs-x[2])/x[3], df=x[1]))-log(x[3]))
}

"score.t" <-
function(x,obs) {
    x     <- as.vector(x)
    obs   <- as.vector(obs)
    names(x) <- names(obs) <- NULL
    n     <- length(obs)
    v     <- x[1]
    v3    <- (v+1.0)/2.0
    v4    <- 1.0/v
    mu    <- x[2]
    sigma <- x[3]
    obs1   <- ((obs - mu)  )/(sigma^2)
    obs2   <- ((obs - mu)^2)/(sigma^2)
    quantityone <- sum(obs2/(v + obs2))
    quantitytwo <- sum(obs1/(v + obs2))
    
    -c( 0.5*n*(digamma(v3) - digamma(v/2.0))
        - 0.5*n*v4 - 0.5*sum(log(1.0 + (obs2/v)))
            + v3*v4*quantityone,
                (v+1.0)*quantitytwo, 
                    -(n - (v+1.0)*quantityone)/sigma)
}


"info.exp.t" <-
function(x) {
    v     <- x[1]
    mu    <- x[2]
    sigma <- x[3]

    v2 <- v+1.0
    v3 <- v2/2.0
    v4 <- 1.0/v
    v5 <- v+3.0
        
    result <- matrix(NA,3,3)
        
    result[1,1] <- 0.25*(trigamma(v/2.0) - trigamma(v3)) - v4*((1.0/v2) - (0.5/v5))
    result[1,2] <- result[2,1] <- result[2,3] <- result[3,2] <- 0
    result[1,3] <- result[3,1] <- ((1.0/v5) - (1.0/v2))/sigma
    result[2,2] <- (1.0 - 2.0/v5)/(sigma^2)
    result[3,3] <- (2.0 * v)/(sigma^2 * v5)
        
    result          
}


"asy.var.t" <-
function(x)  {
    v     <- x[1]
    mu    <- x[2]
    sigma <- x[3]

    v2 <- v+1.0
    v3 <- v2/2.0
    v4 <- 1.0/v
    v5 <- v+3.0
        
    result <- matrix(NA,3,3)
        
    A <- 0.25*(trigamma(v/2.0) - trigamma(v3)) - v4*(1.0/v2 - 0.5/v5)
    result[1,2] <- result[2,1] <- result[2,3] <- result[3,2] <- 0
    D <- (1.0/v5 - 1.0/v2)/sigma
    B <- (1.0 - 2.0/v5)/(sigma^2)
    C <- (2.0 * v)/(sigma^2 * v5)
        
    result[1,1] <- B*C
    result[1,3] <- result[3,1] <- -D*B
    result[2,2] <- A*C - D^2
    result[3,3] <- A*B
        
    result/(B*(A*C-D^2))    
}

