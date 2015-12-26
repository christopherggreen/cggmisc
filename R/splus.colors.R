splus.colors <-
#
function (i=NULL)
{

	ans <- data.frame(
			COLOR=c(
					rgb(  0,  0,  0,max=255), #  1 black
					rgb(255,  0,  0,max=255), #  2 red
					rgb(  0, 64,128,max=255), #  3 navy
					rgb(  0,255,  0,max=255), #  4 green
					rgb(255,127,  0,max=255), #  5 orange
					rgb(  0,126,255,max=255), #  6 sky blue
					rgb(255,255,  0,max=255), #  7 yellow
					rgb(255,  0,  0,max=255), #  8 red
					rgb(198,255,255,max=255), #  9 pastel seagreen
					rgb(255,195,255,max=255), # 10 pastel lavender
					rgb(200,255,200,max=255), # 11 pastel light green
					rgb(255,209,143,max=255), # 12 pastel light orange
					rgb(169,226,255,max=255), # 13 pastel light blue
					rgb(255,255,195,max=255), # 14 pastel light yellow
					rgb(255,140,138,max=255), # 15 pastel dark pink
					rgb(110,110,100,max=255), # 16 dark grey
					rgb(128,128,128,max=255)  # 17 darker grey
			),
			NAME=c(
				"black","red","navy","green","orange","sky blue",
				"yellow","red","pastel seagreen","pastel lavender",
				"pastel light green","pastel light orange",
				"pastel light blue","paste light yellow",
				"pastel dark pink","dark grey","darker grey"
			),
			stringsAsFactors=FALSE
	)

	if ( is.null(i) )
		ans
	else
		ans[i,,drop=FALSE]
}
