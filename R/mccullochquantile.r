"mccullochquantile" <- function(x,symm=FALSE)
{
	qx <- quantile(x,c(0.05,0.25,0.50,0.75,0.95),na.rm=TRUE)
	names(qx) <- NULL
	quant <- as.list(.C("mccullochquantile", 
		quant=as.double(qx), len=as.integer(length(qx)), 
		symm=as.integer(symm), symmlen=as.integer(1), package="cggmisc")$quant)
	names(quant) <- c("alpha","beta","scale","location","zlocation")
	quant	
}
