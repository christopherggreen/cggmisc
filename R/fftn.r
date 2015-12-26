"fftn" <- 
#
# fftn
# 
# Performs n-point FFT analoguous to MATLAB's fft(x,n) 
# 
# If the length of x is < n, x is padded with trailing zeros to length n. 
# If the length of X is greater than n, the sequence X is truncated. 
# When X is a matrix, the length of the columns are adjusted in the same manner.
#
# Christopher G. Green

function(x,n=length(x)) {
	
	if (is.vector(x)) {
		nn <- length(x)
		x <- if ( nn <= n ) c(x,rep(0,n-nn)) else x[1:n]
	}
	else if (is.array(x)) {
		nn <- nrow(x)
		x <- if ( nn <= n ) rbind(x,matrix(0,nrow=n-nn,ncol=ncol(x))) else x[1:n,]
	}
	fft(x)
}
	
	
	
