# RCG BA subgroup work on Sampling PLan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019
		# func "checkData" is a development of original work done by Alastair Pout (Marine Scotland, UK) during project fishpi2

 
rm(list=ls())
library(data.table)
library(fishPiCodes)	
library(foreign) 

# ========================
# reads in data
# ========================
 
 
 # read file names
	file_dnk <- "data\\original\\DNK_V1_01_05.Rdata"
 
# read data
	load(file_dnk)
	dt_dnk<-as.data.table(dat9); rm(dat9)
	
	
# ========================
# check and fix data 	
# ========================
	
	source("funs\\func_checkData.r")
	x<-checkData(x = as.data.frame(dt_dnk), ignore_stop=TRUE)
	
# major: trips with more than one landing date, trips:
	# DNK20170005731 DNK20180080369 DNK20180080543 DNK20180081467 DNK20170146233 DNK20170326720 DNK20170331075 DNK20170331080 DNK20180334066 DNK20170338627 DNK20180497547 DNK20170588180 DNK20170589965 DNK20170598462 DNK20170728240 DNK20170795606 DNK20170795977 DNK20170796190 DNK20170796678

	# quick fix assigns latest landDate
		ls1<-split(dt_dnk, dt_dnk$fishTripId)
		ls2<-lapply(ls1, function(x){if (length(unique(x$landDate))>1) { 
				x$landDate <- as.character(max(as.Date(x$landDate)))
				x } else x})
		dt_dnk<-do.call("rbind", ls2)		
	
# quick fix: issues with metiers [non existing in CL]
	dt_dnk$foCatEu6[dt_dnk$foCatEu6=="FPN_MOL_>0_0_0"]<-"MIS_MIS_0_0_0"

# check depLoc unknown		
	# DKQSB	
# check landLoc unknown		
	# DKQSB	
	
# minor: rectangles missing	
	dt_dnk$rect[dt_dnk$rect==""]<-NA
	table(dt_dnk$rect,useNA="al")
	
	dt_dnk<-data.table(checkData(x = as.data.frame(dt_dnk)))

# ========================	
# save data 	
# ========================		
	
	save(dt_dnk, file="data\\prepared\\DNK_prepared.Rdata")


	
	
















		
