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
 	file_est <- "data\\original\\EST_2019_30_04.csv"

# read data
	dt_est<-fread(file_est, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	
	
# ========================
# check and fix data 	
# ========================
		
	source("funs\\func_checkData.r")		
	checkData(x = as.data.frame(dt_est), ignore_stop=TRUE)

#major: has vslLenCls "" or NA
	sum(is.na(dt_est$vslLenCls) | dt_est$vslLenCls=="") # n= 177
	# fix:
	teste <- is.na(dt_est$vslLenCls) | dt_est$vslLenCls==""
	table(dt_est$foCatEu6[teste])
		# only passive - will not matter
			# quick fix for sake of meeting checking script requirements
				ref_table<-dt_est[vslLenCls!="",.N, list(vslId,vslLenCls)]
				dt_est$vslLenCls[teste]<-ref_table$vslLenCls[match(dt_est$vslId[teste],ref_table$vslId)]
				teste <- is.na(dt_est$vslLenCls) | dt_est$vslLenCls==""
				dt_est$vslLenCls[teste]<-"VL0008" # assumed
		
#major: has depDate "" or NA
	sum(is.na(dt_est$depDate) | dt_est$depDate=="") # n= 3910
	# fix:
	teste <- is.na(dt_est$depDate) | dt_est$depDate==""
		table(dt_est$foCatEu6[teste])
		# only passive - will not matter
			# quick fix for sake of meeting checking script requirements
			dt_est$depDate[teste & substring(dt_est$fishTripId, 1, 7)=="EST2017"]<-"2017-01-01" # default based on year
			dt_est$depDate[teste & substring(dt_est$fishTripId, 1, 7)=="EST2018"]<-"2018-01-01" # default based on year
		
	
#major: has landDate "" or NA
	sum(is.na(dt_est$landDate) | dt_est$landDate=="") # n= 3910
	# fix:
	teste <- is.na(dt_est$landDate) | dt_est$landDate==""
	table(dt_est$foCatEu6[teste])
		# only passive - will not matter
			# quick fix for sake of meeting checking script requirements
			dt_est$landDate[teste & substring(dt_est$fishTripId, 1, 7)=="EST2017"]<-"2017-01-01" # default based on year
			dt_est$landDate[teste & substring(dt_est$fishTripId, 1, 7)=="EST2018"]<-"2018-01-01" # default based on year
	
	table(dt_est$foCatEu6)
	dt_est$foCatEu6[dt_est$foCatEu6=="OTM_SPF_16_104_0_0"]<-"OTM_SPF_16-104_0_0"
	
# major: departures before landing
	teste <- as.Date(dt_est$depDate)-as.Date(dt_est$landDate)>0
	sum(teste)
		# quick fix
		dt_est$depDate[teste]<-dt_est$landDate[teste]	
	
# check depLoc unknown		 
	FINKNA
# check landLoc unknown
	EEHDI EELYK EENAY EENEE EEKKR EEKMM EEPTK EEPRZ EETJS EEMVS EEPUK EELIY EEPUL
# landCat unknown		 
	"" # 815
	
	dt_est<-checkData(x = as.data.frame(dt_est))

# ========================	
# save data 	
# ========================		
	
save(dt_est, file="data\\prepared\\EST_prepared.Rdata")














		
