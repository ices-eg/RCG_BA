legend_grid2 <- function (x, y = NULL, breaks, col, digits = 2, suffix = "", 
    type = 1, pch = 15, pt.cex = 2.5, bg = "lightblue", ...) 
{
	
	# Original code: Hans Gerritsen @ mapplots:legend.grid
	# Nuno Prista: modification of original code to allow specification of pt.bg when pch %in% c(21:25)

    ncol <- length(breaks) - 1
    if (missing(col)) 
        col <- colorRampPalette(c("lightyellow", "yellow", "orange", 
            "red", "brown4"))(ncol)
    tempfun <- function(x) format(x, digits = digits)
    min <- sapply(breaks[(ncol):1], tempfun)
    mid <- sapply((breaks[(ncol):1] + breaks[(ncol + 1):2])/2, 
        tempfun)
    max <- sapply(breaks[(ncol + 1):2], tempfun)
    if (type == 1) 
        legend <- paste(mid, suffix, sep = "")
    if (type == 2) 
        legend <- paste(min, " - ", max, suffix, sep = "")
    if(pch %in% 1:15)
		{legend(x, y, legend = legend, col = col[ncol:1], pch = pch, 
			pt.cex = pt.cex, bg = bg, ...)
		} else {
			legend(x, y, legend = legend, pt.bg = col[ncol:1], col=1, pch = pch, 
				pt.cex = pt.cex, bg = bg, ...)
				}	
}