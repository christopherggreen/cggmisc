polish <- 
# 
# calculate mean or median polish
#
# Christopher G. Green
#
# 2016-08-21
#
#
function( mat, fun=c("mean","median"), do.print=FALSE, max.iter=3 ) {

	mat <- as.matrix( mat )
	fun <- get(match.arg(fun))
	flag.done <- FALSE
	rowstat.final <- rep(NA, dim(mat)[1]) 
	colstat.final <- rep(NA, dim(mat)[2]) 

	for ( iter in 1:max.iter ) 
	{
		# calculate row summaries
		rowstat <- apply( mat, 1, fun, na.rm=TRUE )
		colstat <- apply( mat, 2, fun, na.rm=TRUE )

		rowstat.final <- if ( iter == 1 ) rowstat else rowstat.final + rowstat
		colstat.final <- if ( iter == 1 ) colstat else colstat.final + colstat

		# check if another round is needed in the while condition
		if ( all( rowstat == rep(0, dim(mat)[1]), na.rm=TRUE ) &&
		     all( colstat == rep(0, dim(mat)[2]), na.rm=TRUE ) )
		{
			 	flag.done <- TRUE
				break
		}

		if ( do.print ) {
			cat(" After row summary: \n" )
			print( cbind( mat, rowstat) )
		}
		# sweep out row summaries
		mat <- sweep( mat, 1, STAT=rowstat, FUN="-" )
		# append row summaries
		# mat <- cbind( mat, rowstat )

		# calculate col summaries
		colstat <- apply( mat, 2, fun, na.rm=TRUE )
		if ( do.print ) {
			cat(" After col summary: \n" )
			print( rbind( mat, colstat) )
		}
		# sweep out col summaries
		mat <- sweep( mat, 2, STAT=colstat, FUN="-" )
	}

	list(mat=mat, rowstat=rowstat.final, colstat=colstat.final)
}

