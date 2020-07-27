heatmap_ices_rect_one_var <- function(x, Var, var1, F, map_title, legend_title, legend_amp, nbreaks, quantile1=0.975, xlim = c(-42,+5), ylim = c(32.75,63.25), legend_Var_scale=1, map_list = "", suffix =""){	
	# Nuno Prista, SLU, Sweden
	# Developed @ RCM NS&EA 2017-18, RCG subgroup work 2019
	
	# draws grid map with heat-mapped ices rectangles
	# x is a data.frame where Var is integer/numeric and var1 are ices rectangles). 
	# F is the function that is applied to on each ices rectangle (e.g., sum, mean)
	# xlim and ylim help define map range
	# legend_title, legend_Var_scale and suffix are for legend only. Note: use legend.amp = 100 if Var data is percentage

	#2017/09/11-15: first version develop during RCG NSEA 2017
	#2018/09/03-10: improvement during RCG NSEA 2018
	#2019/04/02-06: significant upgrade of RCG NSEA 2018 function:
					# bugs corrected
					# improved display of ices rectangles (shapefile avoids display outside target area)
					# inclusion of percent coverages and map subtitle
					# added argument "quantile1"
					# added argument "map_list"
					# improved annotation
					
	#browser()
	percent_Var <- round(sum(!is.na(x[,Var]))/dim(x)[1]*100,2)
	percent_var1 <- round(sum(!is.na(x[,var1]) & !x[,var1]=="99u9")/dim(x)[1]*100,2)
	
	x<-x[!is.na(x[,var1]) & !x[,var1]=="99u9",]
	ls1<-split(x, factor(x[,var1]))
	ls2<-lapply(ls1, function(x){data.frame(StatisticalRectangle = x[,var1][1], value = F(x[[Var]], na.rm=T))})
	df1<-do.call("rbind", ls2)
	df1<-data.frame(df1, ices.rect(as.character(df1$StatisticalRectangle)), row.names=NULL)
	
	Var_in_grid <- sum(df1$value[df1$StatisticalRectangle %in% map_ices_rect$dbf$dbf$ICESNAME])/sum(df1$value)
	percent_Var_in_grid <- round(sum(df1$value[df1$StatisticalRectangle %in% map_ices_rect$dbf$dbf$ICESNAME])/sum(df1$value)*100,2)
	percent_Var_outside_grid <- 100-percent_Var_in_grid
	
	# restrict values to grid
	df1<-droplevels(df1[df1$StatisticalRectangle %in% as.character(map_ices_rect$dbf$dbf$ICESNAME),])
	
	# determines grid and breaks
	byy = 0.5; byx = 1;
	grd<-make.grid(x = df1$lon, y = df1$lat, z = df1$value, byx = byx, byy = byy, xlim = xlim, ylim = ylim)
	breaks <- breaks.grid(grd, quantile1, ncol=nbreaks, zero=TRUE)
	print(quantile(grd, probs=c(0,0.01, 0.025, 0.975, 0.99, 1), na.rm=TRUE))
	print(breaks)
	
	# plots maps
	basemap(xlim=xlim, ylim=ylim, main = "", bg="white", xaxs="i", yaxs="i")
	draw.grid(grd,breaks)
	if("map_ices_rect" %in% names(map_list)) draw.shape(map_ices_rect, border="gray", col="transparent")
	if("map_fao_areas" %in% names(map_list)) draw.shape(map_fao_areas, border="black", col="transparent")
	if("map_coast" %in% names(map_list)) draw.shape(map_coast, col="beige")
	if("map_borders" %in% names(map_list)) draw.shape(map_borders, type="l", col="black")	
	# adds titles
	title(map_title, line=2)
	title(main=paste("with main_var: ", percent_Var,"%; with rect: ",percent_var1,"%; total main_var_in_grid: ", percent_Var_in_grid, sep=""), cex.main=0.8, line = 1)
	# adds legend
	legend_grid2(x="topleft", breaks=breaks*legend_Var_scale, pch= 22, pt.cex=1.7, cex=0.7, type=2, inset=0.02, title=legend_title, bg="white", suffix = suffix)
	
	# example: heatmap_ices_rect_one_var(x = as.data.frame(ce_rcg), Var = "KWDays", var1 = "StatisticalRectangle", F=sum, map_title = "lol", legend_title = "lol", nbreaks = 6, xlim=c(-45.5,10.5), ylim=c(32.25,65.5), legend_amp=1, map_list = map_list)

	}