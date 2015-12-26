splus.theme <-
#
function (name = .Device, color = (name != "postscript") )
{

	# lower.saturation function from the lattice source code (not exported)
		lower.saturation <- function(x, f = 0.2)
		{
			 rgb <- col2rgb(x)
			 rgb[] <- 255 - rgb
			 rgb[] <- round(f * rgb)
			 rgb[] <- 255 - rgb
			 rgb(rgb["red", ],
				  rgb["green", ],
				  rgb["blue", ],
				  maxColorValue = 255)
		}

	 #print(color)
    if (color) {
        can.col <- if (name %in% c("windows", "X11","postscript","pdf")) 
		  		splus.colors()[,1,drop=TRUE]
        else c("#000000", "#00FFFF", "#FF00FF", "#00FF00", "#FF7F00", 
            "#007EFF", "#FFFF00", "#FF0000", "#C6FFFF", "#FFC3FF", 
            "#C8FFC8", "#FFD18F", "#A9E2FF", "#FFFFC3", "#FF8C8A", 
            "#AAAAAA", "#909090")
    }
    else can.col <- c("#000000", "#999999", "#4C4C4C", "#E6E6E6", 
        "#F2F2F2", "#B2B2B2", "#000000", "#030303", "#050505", 
        "#080808", "#0A0A0A", "#0D0D0D", "#0F0F0F", "#121212", 
        "#151515", "#AAAAAA", "transparent")


		ans <- list(
			grid.pars = list(), 
			fontsize = list(text = 12, points = 8), 
			background = list(alpha = 1, col = rgb(255,255,255,max=255)), 
			clip = list(panel = "on", strip = "on"), 
			add.line = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1), 
			add.text = list(alpha = 1, cex = 1, col = can.col[1], font = 1, lineheight = 1.2), 
        	plot.polygon = list(
				alpha = 1, col = can.col[2], border = "black", lty = 1, lwd = 1), 
			box.dot = list(alpha = 1, col = can.col[1], 
            cex = 1, font = 1, pch = 16), 
			box.rectangle = list(alpha = 1, 
            col = can.col[2], fill = "transparent", lty = 1, lwd = 1), 
			box.umbrella = list(alpha = 1, col = can.col[2], 
            lty = 2, lwd = 1), 
			dot.line = list(alpha = 1, col = can.col[16], 
            lty = 1, lwd = 1), 
			dot.symbol = list(alpha = 1, cex = 0.8, 
            col = can.col[2], font = 1, pch = 16), 
			plot.line = list(alpha = 1, 
            col = can.col[2], lty = 1, lwd = 1), 
			plot.symbol = list(alpha = 1, 
            cex = 0.75, col = can.col[2], font = 1, pch = 1, fill = "transparent"), 
        	reference.line = list(alpha = 1, col = can.col[16], lty = 1, 
            lwd = 1), 
		  	strip.background = list(alpha = 1, col = can.col[c(12, 
            11, 9, 13, 10, 15, 14)]), 
			strip.shingle = list(alpha = 1, col = can.col[c(5, 4, 2, 6, 3, 8, 7)]), 
			strip.border = list(alpha = 1, col = rep(can.col[1], 7), lty = rep(1, 7), 
				lwd = rep(1, 7)), 
			superpose.line = list(alpha = 1, col = can.col[2:8], 
            lty = rep(1, 7), lwd = rep(1, 7)), 
			superpose.symbol = list(alpha = rep(1, 
            7), cex = rep(0.8, 7), col = can.col[2:8], fill = lower.saturation(can.col[2:8]), 
            font = rep(1, 7), pch = rep(1, 7)), 
			superpose.polygon = list(alpha = rep(1, 
            7), col = lower.saturation(can.col[2:8]), border = rep("black", 
            7), lty = rep(1, 7), lwd = rep(1, 7)), 
			regions = list(alpha = 1, col = rev(cm.colors(100))), 
			shade.colors = list(alpha = 1, 
            palette = function(irr, ref, height, saturation = 0.9) {
                hsv(h = height, s = 1 - saturation * (1 - (1 - ref)^0.5), v = irr)
            }), 
			axis.line = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1), 
			axis.text = list(alpha = 1, cex = 0.8, col = can.col[1], font = 1), 
			axis.components = list(
					left = list(tck = 1, pad1 = 1, pad2 = 1), 
					top = list(tck = 1, pad1 = 1, pad2 = 1), 
					right = list(tck = 1, pad1 = 1, pad2 = 1), 
					bottom = list(tck = 1, pad1 = 1, pad2 = 1)
				), 
			layout.heights = list(
					top.padding = 1, main = 1, main.key.padding = 1, 
					key.top = 1, key.axis.padding = 1, axis.top = 1, 
					strip = 1, panel = 1, axis.panel = 1, between = 1, 
					axis.bottom = 1, axis.xlab.padding = 1, xlab = 1, 
					xlab.key.padding = 1, key.bottom = 1, key.sub.padding = 1, 
					sub = 1, bottom.padding = 1
				), 
			layout.widths = list(
					left.padding = 1, key.left = 1, key.ylab.padding = 1, 
					ylab = 1, ylab.axis.padding = 1, axis.left = 1, 
					axis.panel = 1, strip.left = 1, panel = 1, between = 1, 
					axis.right = 1, axis.key.padding = 1, key.right = 1, 
					right.padding = 1
				), 
			box.3d = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1), 
			par.xlab.text = list(alpha = 1, cex = 1, col = can.col[1], 
				font = 1, lineheight = 1), 
        	par.ylab.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 1, lineheight = 1), 
			par.zlab.text = list(alpha = 1, cex = 1, col = can.col[1], 
				font = 1, lineheight = 1), 
        	par.main.text = list(alpha = 1, cex = 1.2, col = can.col[1], 
            font = 2, lineheight = 1), 
			par.sub.text = list(alpha = 1, cex = 1, col = can.col[1], 
				font = 2, lineheight = 1)
		)
    if (color) {
        if (name == "postscript" || name == "pdf") {
            ans$plot.symbol$col 						<- can.col[6]
            ans$plot.line$col 						<- can.col[6]
            ans$dot.symbol$col 						<- can.col[6]
            ans$box.rectangle$col 					<- can.col[6]
            ans$box.umbrella$col 					<- can.col[6]
            ans$superpose.symbol$col 				<- 
					c(can.col[c(6, 3, 4, 8)], "orange", "darkgreen", "brown")
            ans$superpose.symbol$col[c(3, 6)] 	<- 
					ans$superpose.symbol$col[c(6, 3)]
            ans$superpose.line$col 					<- ans$superpose.symbol$col
        }
    } else {
        ans$plot.polygon$col 							<- can.col[5]
        ans$box.rectangle$col 						<- can.col[1]
        ans$box.umbrella$col 							<- can.col[1]
        ans$dot.line$col 								<- can.col[4]
        ans$dot.symbol$col 							<- can.col[1]
        ans$plot.line$col 								<- can.col[1]
        ans$plot.symbol$col 							<- can.col[1]
        ans$regions$col 								<- 
		  	grey(seq(0.3^2.2, 0.9^2.2, length.out = 100)^(1/2.2))
        ans$shade.colors$palette 					<- 
		  	function(irr, ref, height, w = 0.5) 
				grey(w * irr + (1 - w) * (1 - (1 - ref)^0.4))
        ans$strip.background$col 					<- can.col[rep(5, 7)]
        ans$strip.shingle$col 						<- can.col[rep(6, 7)]
        ans$superpose.line$col 						<- can.col[rep(1, 7)]
        ans$superpose.line$lty 						<- 1:7
        ans$superpose.symbol$col 					<- can.col[rep(1, 7)]
        ans$superpose.symbol$cex 					<- rep(0.7, 7)
        ans$superpose.symbol$pch 					<- c(1, 3, 6, 0, 5, 16, 17)
        ans$superpose.polygon$col 					<- 
		  	grey((c(6, 12, 7, 11, 8, 10, 9)/15)^0.8)
    }
    ans
}
