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
	file_ltu <- "data\\original\\LTU_2019_29_04.csv"
 
# read data
	dt_ltu<-fread(file_ltu, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")

# ========================
# check and fix data 	
# ========================

	source("funs\\func_checkData.r")
	checkData(x = as.data.frame(dt_ltu), ignore_stop=TRUE)

# minor: rename columns
	dt_ltu$vslLenCls <- dt_ltu$lenghtsegmnt; dt_ltu$lenghtsegmnt<-NULL
	dt_ltu$landCat <- dt_ltu$LangCAT01; dt_ltu$LangCAT01<-NULL

# minor: issues with formating			
	dt_ltu$sppCode <- as.integer(dt_ltu$sppCode)
	dt_ltu$landWt<-as.numeric(dt_ltu$landWt)

# major: issues with metiers [non existing in CL]	
	dt_ltu$foCatEu6[dt_ltu$foCatEu6=="GNS_SPF_16-31_0_0"]<-"GNS_SPF_16-109_0_0"	

# check ERROR more than one departure date, trips:
	# LTU20170000159 LTU20170000579 LTU20170000739 LTU20170001189
	# fix: 
		dt_ltu$depDate[dt_ltu$fishTripId=="LTU20170000043"]<-"2017-01-16"
		dt_ltu$depDate[dt_ltu$fishTripId=="LTU20170000155"]<-"2017-05-06"
		dt_ltu$depDate[dt_ltu$fishTripId=="LTU20170000159"]<-"2017-05-16"
		dt_ltu$depDate[dt_ltu$fishTripId=="LTU20170000579"]<-"2017-03-23"
		dt_ltu$depDate[dt_ltu$fishTripId=="LTU20170000739"]<-"2017-05-06"
		dt_ltu$depDate[dt_ltu$fishTripId=="LTU20170001189"]<-"2017-12-21"

	
# major:  "27.3.d.28" area not in the code list
		sum(dt_ltu$area=="27.3.d.28") # n = 449
		# fix
		dt_ltu$area[dt_ltu$area=="27.3.d.28"]<-"27.3.d.28.2" # according to CL
	
	dt_ltu<-data.table(checkData(x = as.data.frame(dt_ltu)))

# ========================	
# save data 	
# ========================		
	
	save(dt_ltu, file="data\\prepared\\LTU.Rdata")

	
	














		
