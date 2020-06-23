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
 file_fin <- "data\\original\\FIN.csv"

# read data
	dt_fin<-fread(file_fin, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")

	
# ========================
# check and fix data 	
# ========================
	
	source("funs\\func_checkData.r")	
	checkData(x = as.data.frame(dt_fin), ignore_stop=TRUE)
		
# minor: rename columns
	dt_fin$fishTripId <- dt_fin$fishTripid; dt_fin$fishTripid<-NULL
	
# minor: issues with formating	
	dt_fin$landWt<-as.numeric(dt_fin$landWt)
	dt_fin$depLoc<-as.character(dt_fin$depLoc)
	
# minor: issues with metiers [non existing in CL]
	dt_fin$foCatEu6[dt_fin$foCatEu6=="OTM_DEF_>=105_1_12"]<-"OTM_DEF_>=105_1_120"
	
#major: all depLoc "" or NA
	sum(is.na(dt_fin$depLoc) | dt_fin$depLoc=="") # n= 10689
	teste <-  is.na(dt_fin$depLoc) | dt_fin$depLoc==""
	dt_fin[teste,]
	# fix:
		#assumed landLoc == depLoc
		dt_fin$depLoc[teste]<-dt_fin$landLoc[teste]
	sum(is.na(dt_fin$depLoc) | dt_fin$depLoc=="") # n= 10689
	
#minor: landing before departure, trips:
	dt_fin$depDate[dt_fin$fishTripId=="FIN20170003176"]<-"2017-02-02" # based on activity patterns
	
#check: Days at sea greater than 20, for  4 trips:
	# FIN20180001365 FIN20180001700 FIN20180003475 FIN20170003468 

		dt_fin<-data.table(checkData(x = as.data.frame(dt_fin)))

		
# ========================	
# save data 	
# ========================	
		
save(dt_fin, file="data\\prepared\\FIN_prepared.Rdata")





















		
