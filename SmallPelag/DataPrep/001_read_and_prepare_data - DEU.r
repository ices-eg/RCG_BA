# RCG BA subgroup work on Sampling PLan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019
		# func "checkData" is a development of original work done by Alastair Pout (Marine Scotland, UK) during project fishpi2

 
 
rm(list=ls())
library(data.table)
library(fishPiCodes)	
library(foreign) 

# ========================
# read in data
# ========================
 
 # read file names
 	file_deu <- "data\\original\\DEU_2019_06_06.csv"
 
# read data
	dt_deu<-fread(file_deu, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")


# ========================	
# check and fix data 	
# ========================	
	
	source("funs/func_checkData.r")				
	checkData (x = as.data.frame(dt_deu), ignore_stop=TRUE)
	
# minor: rename columns
	dt_deu$vslFlgCtry <- dt_deu$vslFglCtry; dt_deu$vslFglCtry<-NULL
	dt_deu$landWt <- dt_deu$LandWt; dt_deu$LandWt<-NULL
# minor issue with fishTripId
	# adding 00 to end of fishTripId to meet character number
	dt_deu$fishTripId<-paste(dt_deu$fishTripId,"00", sep="")
# minor: issues with sppCodes	
	dt_deu$spp_Code_aphia[dt_deu$spp_Code_aphia == 0 & dt_deu$sppName == "Prionotus spp"] <- 159569; 
	dt_deu$spp_Code_aphia[dt_deu$spp_Code_aphia == 0 & dt_deu$sppCode == "SRA"] <- 159569; 
	dt_deu$sppName[dt_deu$spp_Code_aphia == 159569 & dt_deu$sppCode == "SRA"] <- "Prionotus spp"; 
	dt_deu$spp_Code_aphia[dt_deu$spp_Code_aphia == 0 & dt_deu$sppName == "Thunnini"] <- 125559; 
	dt_deu$sppName[dt_deu$sppName == "Thunnini"] <- "Scombridae"; 
	dt_deu$sppCode<-dt_deu$spp_Code_aphia
# minor: issues with formating	
	dt_deu$landWt<-as.numeric(dt_deu$landWt)
	dt_deu$sppCode <- as.integer(dt_deu$sppCode)
# minor: issues with spp names and codes
	dt_deu$sppName[dt_deu$sppName=="Prionotus spp"] <- "Prionotus"
	dt_deu$sppName[dt_deu$sppName=="Trachurus spp"] <- "Trachurus"
	dt_deu$sppName[dt_deu$sppName=="Salmo spp"] <- "Salmo"
	dt_deu$sppName[dt_deu$sppName=="Stizostedion lucioperca"] <- "Sander lucioperca"
	dt_deu$sppName[dt_deu$sppName=="Abramis spp"] <- "Abramis"
# minor: issues with areas
	dt_deu$area[dt_deu$area=="27.3.a.n"]<-"27.3.a.20"
	
# major: vslLenCls "" or NA
	teste <- is.na(dt_deu$vslLenCls) | dt_deu$vslLenCls==""
	sum(teste) # n = 10
	dt_deu[teste,]
		# fix:
			dt_deu[dt_deu$vslId=="DEU167287",]
			dt_deu[dt_deu$vslId=="DEU167287","vslLenCls"]<-"VL40XX"
			dt_deu[dt_deu$vslId=="DEU47577",]
			dt_deu[dt_deu$vslId=="DEU47577","vslLenCls"]<-"VL1218"
			dt_deu[dt_deu$vslId=="DEU91389",]
			dt_deu[dt_deu$vslId=="DEU91389","vslLenCls"]<-"VL40XX"
			# assumed based on trip duration: table(as.Date(dt_deu$landDate)-as.Date(dt_deu$depDate),dt_deu$vslLenCls)
				dt_deu[dt_deu$vslId=="DEU29658",]
				dt_deu[dt_deu$vslId=="DEU29658","vslLenCls"]<-"VL0008"
	teste<-is.na(dt_deu$vslLenCls) | dt_deu$vslLenCls==""
	sum(teste) # n = 0
			
# major: depDate "" or NA
	teste<-is.na(dt_deu$depDate) | dt_deu$depDate==""
	sum(teste) # n = 5
	dt_deu[teste,]
		# note: not very important as passive will not be on the sampling frame
	dt_deu$depDate[teste]<-"2018-01-01" # default based on year
	dt_deu$landDate[teste]<-"2018-01-01" # default based on year
	teste<-is.na(dt_deu$depDate) | dt_deu$depDate==""
	sum(teste) # n = 0
	
# major: foCatEu6 "" or NA
	teste<-is.na(dt_deu$foCatEu6) | dt_deu$foCatEu6==""
	sum(teste) # n = 15
	dt_deu[teste,]
	dt_deu[vslId=="DEU167287" & foCatEu6=="","foCatEu6"]<-"OTM_SPF_32-69_0_0" # based on CL. Also evaluated SWE and DNK data, same vessel size and same area, crossed with dominant gear of the specific DEU vessels
	dt_deu[vslId=="DEU91389" & foCatEu6=="","foCatEu6"]<-"OTM_SPF_32-69_0_0" # based on CL. Also evaluated SWE and DNK data, same vessel size and same area, crossed with dominant gear of the specific DEU vessels
	dt_deu[vslId=="DEU47577" & foCatEu6=="","foCatEu6"]<-"PTM_SPF_32-104_0_0" # based on activity of vessel
	dt_deu[vslId=="DEU29658" & foCatEu6=="","foCatEu6"]<-"GNS_SPF_32-109_0_0" # based on likelihood (large catches of HER from 38G3 and VL0008
	teste<-is.na(dt_deu$foCatEu6) | dt_deu$foCatEu6==""
	sum(teste) # n = 0
	
# major: issues with metiers [non existing in CL]
	dt_deu$foCatEu6<-as.character(dt_deu$foCatEu6)		
	dt_deu$foCatEu6[dt_deu$foCatEu6=="PTM_SPF_32-109_0_0"]<-"PTM_SPF_32-104_0_0" # 20 records
	dt_deu$foCatEu6[dt_deu$foCatEu6=="GNS_SPF_16-32_0_0"]<-"GNS_SPF_16-109_0_0" # 44 records
	dt_deu$foCatEu6[dt_deu$foCatEu6=="PTB_SPF_32-109_0_0"]<-"PTB_SPF_32-104_0_0" # 3 records
	
# major: landLoc or depLoc "" or NA
	sum(is.na(dt_deu$landLoc) | dt_deu$landLoc=="") # n = 5
		teste<-is.na(dt_deu$landLoc) | dt_deu$landLoc=="" | is.na(dt_deu$depLoc) | dt_deu$depLoc==""
		sum(teste) # n = 5
		dt_deu[teste,]
		dt_deu$landLoc[dt_deu$vslId == "DEU24876" & dt_deu$landLoc==""]<-"DELAK" # based on vessel record
		dt_deu$depLoc[dt_deu$vslId == "DEU24876" & dt_deu$depLoc==""]<-"DELAK" # based on vessel record
		dt_deu$landLoc[dt_deu$vslId == "DEU32815" & dt_deu$landLoc==""]<-"DEUMZ" # based on vessel record
		dt_deu$depLoc[dt_deu$vslId == "DEU32815" & dt_deu$depLoc==""]<-"DEUMZ" # based on vessel record
		dt_deu$landLoc[dt_deu$vslId == "DEU66019" & dt_deu$landLoc==""]<-"DE8AN" # based on vessel record
		dt_deu$depLoc[dt_deu$vslId == "DEU66019" & dt_deu$depLoc==""]<-"DE8AN" # based on vessel record
		teste<-is.na(dt_deu$landLoc) | dt_deu$landLoc==""
		sum(teste) # n = 0
		
# major: landLoc unknown	
	#DEND1
	#DEBR1
# major: depLoc unknown		 
	#DEND1
	#DEBR1
# major: landCat unknown		 
	#HCN # 12423
	#UKN # 5078
		dt_deu$landCat[dt_deu$landCat=="HCN"]<-"HUC"
		dt_deu$landCat[dt_deu$landCat=="UKN"]<-NA

	dt_deu<-data.table(checkData(x = as.data.frame(dt_deu)))

# ========================	
# save data 	
# ========================		
	
	save(dt_deu, file="data\\prepared\\DEU_prepared.Rdata")


	
















		
