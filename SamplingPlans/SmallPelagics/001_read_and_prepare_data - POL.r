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
 	file_pol <- "data\\original\\POL_2019_10_05.Rdata"
	
# read data
	load(file_pol)
	dt_pol<-as.data.table(result); rm(result)

	
# ========================
# check and fix data 	
# ========================		

	source("funs\\func_checkData.r")
	x<-checkData(x = as.data.frame(dt_pol), ignore_stop=TRUE)

# minor: issues with formating	
	dt_pol$sppCode<-as.integer(dt_pol$sppCode)
	#dt_pol$sppName<-as.character(dt_pol$sppName)
	#dt_pol$landWt<-as.numeric(gsub(",",".", dt_pol$landWt))
		
#major depDate "" or NA
	teste <- is.na(dt_pol$depDate)
	sum(teste) # n= 7679
	dt_pol[teste,]
	# only passive - will not matter
		# quick fix for sake of meeting checking script requirements
		dt_pol$depDate[teste]<-dt_pol$landDate[teste]
	
	dt_pol<-data.table(checkData(x = as.data.frame(dt_pol)))

# ========================	
# save data 	
# ========================		
	
	save(dt_pol, file="data\\prepared\\POL_prepared.Rdata")
				
	

















		
