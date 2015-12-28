"fit.mle.stable" <- 
#
#
function(obsvals, method = c("nlminb","optim"),
	mean.exists=TRUE, robust.location=FALSE, strace=FALSE) {

	method <- match.arg(method)

	# Compute initial estimates of the parameters
	initvals <- mccullochquantile(obsvals)[1:4]
	if ( robust.location ) initvals[4] <- location.m(obsvals)

	if (initvals$alpha == 2) {
		initvals$alpha <- 1.99
		warning("Initial Alpha altered additionally")
	}
	if (abs(initvals$beta) == 1) {
		initvals$beta <- sign(initvals$beta)*0.95
		warning("Initial Beta altered additionally")
	}
	# 05/25/2007 sometimes when the mean exists and the true alpha is near 1 the 
	# initial value will be less than 1
	if ( mean.exists ) initvals$alpha <- max(initvals$alpha, 1+1e-6)
	
	initvals <- unlist(initvals)
	names(initvals) <- NULL	
	
	result.old <- forward.transform.parameters(initvals,mean.exists=mean.exists)

 	cat("Optimization starting from ", initvals, "(transformed: ", result.old, ")\n")

	if ( method == "nlminb" ) {
		result <- nlminb(result.old, objective=likelihood.stable,
			control=list(rel.tol=1e-6),
			obs=obsvals, log2N=16, mean.exists=mean.exists)	
		
		if ( length(grep(result$message,"SINGULAR")) 
			|| length(grep(result$message,"FALSE")) ) {
			stop("nlminb failed to converge")
		}
		result$hessian <- NULL
	} else if ( method == "optim" ) {
		result <- optim(result.old, fn=likelihood.stable, method="BFGS", 
			control=list(reltol=1e-6),hessian=TRUE,
		obs=obsvals, log2N=16)
		
		if ( result$convergence ) {
			print(result)
			stop("optim failed to converge")
		}
	} else {
		# stub for future use
	}

	result <- backward.transform.parameters(result$par,mean.exists=mean.exists)

	list("alpha"=result[1],"beta"=result[2],"sigma"=result[3],
		"mu"=result[4],"hessian"=result$hessian)
}


"likelihood.stable" <- function(x, y, obs, mean.exists=TRUE) {

	# Computes the logarithm of the likelihood function
	# multilpied by -1 given a vector of parameters and 
	# a vector of observations (obs)

	# input x is given in terms of transformed variable
	x <- backward.transform.parameters(c(x,y), mean.exists=mean.exists)
	#print(x)

	# use stable from fBasics
		
	-sum(log(dstable(obs, x[1], x[2], x[3], x[4])$f))	
}


"backward.transform.parameters" <- 
#
# transform back from R^4 to usual range
#
# alpha = 1 + \frac{1}{1 + \alpha'^2}
# beta  = \frac{2 - \beta'^2}{2 + \beta'^2}
# \sigma = exp(\sigma')
#
#
function(theta.prime, mean.exists=TRUE) {
  theta.prime[1:2] <- theta.prime[1:2]^2
  if ( mean.exists )
    c(1 + 1/(1 + theta.prime[1]), 1 - 2/((2/theta.prime[2]) + 1), 
      exp(theta.prime[3]), theta.prime[4])
  else
    c(2/(1 + theta.prime[1]), 1 - 2/((2/theta.prime[2]) + 1), 
	   exp(theta.prime[3]), theta.prime[4])
}


"forward.transform.parameters" <- 
#
# transform usual values to R^4 as discussed in Rachev and Mittnik
# 
# 
#
#
function(theta, mean.exists=TRUE) {
	if ( mean.exists )
	  c(sqrt( (2 - theta[1])/(theta[1] - 1) ),
		 sqrt(2 * ((1 - theta[2])/(1 + theta[2]))),log(theta[3]),theta[4])
	else
	  c(sqrt( (2 - theta[1])/theta[1] ),
	    sqrt(2 * ((1 - theta[2])/(1 + theta[2]))),log(theta[3]),theta[4])
}
