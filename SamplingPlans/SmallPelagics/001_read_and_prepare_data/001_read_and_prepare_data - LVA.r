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
 	file_lva1 <- "data\\original\\RCG SUB-GROUP_2017_2018_tables_LV.csv" #2017
	file_lva2 <- "data\\original\\RCG SUB-GROUP_2017_2018_tables_LV2.csv" #2018

# read data
	dt_lva1<-fread(file_lva1, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_lva2<-fread(file_lva2, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_lva <- rbind(dt_lva1, dt_lva2)
	

# ========================
# check and fix data 	
# ========================	
	
	source("funs\\func_checkData.r")
	x<-checkData(x = as.data.frame(dt_lva), ignore_stop=TRUE)

# minor: issues with formating		
	dt_lva$sppCode <- as.integer(dt_lva$sppCode)
	dt_lva$landWt<-as.numeric(dt_lva$landWt)		
	dt_lva$depDate<-as.character(dt_lva$depDate)		
	dt_lva$depLoc<-as.character(dt_lva$depLoc)		
	dt_lva$rect<-as.character(dt_lva$rect)		
# minor: landDate wrong format	
	ls1<-strsplit(dt_lva$landDate,"-")
	ls2<-lapply(ls1, function(x){if(!is.na(x[3]) & nchar(x[3])==1) {x[3]<-paste(0,x[3],sep="")}; if(!is.na(x[2]) &nchar(x[2])==1) {x[2]<-paste(0,x[2],sep="")}; x})
	df1<-do.call("rbind", ls2)	
	dt_lva$landDate<-c(apply(df1, 1, paste, collapse="-"), NA, NA)
	dt_lva$landWt<-as.numeric(dt_lva$landWt)
# minor: replace: pair (714851,"Clupea harenguss" ) by pair (126417, "Clupea harengus")
	dt_lva$sppCode[dt_lva$sppCode==714851]<-126417
	dt_lva$sppName[dt_lva$sppName=="Clupea harenguss"]<-"Clupea harengus"
# minor: replace: sppName "Osmerus epelanus" by sppName "Osmerus eperlanus")
	dt_lva$sppName[dt_lva$sppName=="Osmerus epelanus"]<-"Osmerus eperlanus"
# minor: replace: sppCode 405822 [Spratella sprattus baltica] by sppCode 126425 [Sprattus sprattus]
	dt_lva$sppCode[dt_lva$sppCode==405822]<-126425
# minor: issues with metiers [non existing in CL]
	dt_lva$foCatEu6[dt_lva$foCatEu6 %in% c("OTM_SPF_16_31_0_0", "OTM_SPF_16_104_0_0")]<- "OTM_SPF_16-104_0_0"	
# minor:	
	dt_lva$sppCode <- as.integer(dt_lva$sppCode)	
	
# major: all depDate "" or NA
	table(dt_lva$depDate, useNA="al")
	table(dt_lva$landDate, useNA="al")
		# quick fix for sake of meeting checking script requirements
		teste<-is.na(dt_lva$depDate) | dt_lva$depDate==""
		dt_lva$depDate[teste]<-dt_lva$landDate[is.na(dt_lva$depDate)]
		teste<-is.na(dt_lva$depDate) | dt_lva$depDate==""
		sum(teste)
		dt_lva$depDate[teste]<-dt_lva$landDate[teste]<-"2018-01-01"
		teste<-is.na(dt_lva$depDate) | dt_lva$depDate==""
		sum(teste)		
		
# major: all depLoc "" or NA
	table(dt_lva$depLoc, useNA="al")
	teste<-is.na(dt_lva$depLoc) | dt_lva$depLoc==""
	# quick fix: 
	dt_lva$depLoc[teste]<-dt_lva$landLoc[teste]

# major: depLoc == "(blank)" | landLoc == "(blank)"
	teste<-dt_lva$depLoc=="(blank)"
	sum(teste)
	# quick fix:
	dt_lva$depLoc[teste]<-dt_lva$landLoc[teste]<-"LVVNT" # based on vessel activity
	
# major trips with two departure dates - corrected to two trips
	prob_trips<-dt_lva[,length(unique(depDate)),fishTripId][V1>1,]$fishTripId
	length(prob_trips)
	# quick fix: 
	dt_lva[fishTripId %in% prob_trips,]
	ls1<-split(dt_lva, dt_lva$fishTripId)
	ls2<-lapply(ls1, function(x){if (length(unique(x$depDate))>1) { 
			x$fishTripId <- paste(substring(x$fishTripId, 1, 7),LETTERS[as.numeric(factor(x$depDate))],substring(x$fishTripId, 9, 14), sep="")
			x } else x})
	dt_lva<-do.call("rbind", ls2)		
	
# check: all rect "" or NA
	table(dt_lva$rect, useNA="al")
	
# check: landCat unknown		 
	table(dt_lva$landCat, useNA="al")

	BMS # 1
	GUT # 1
	WHL	# 25865
	
	
	dt_lva<-data.table(checkData(x = as.data.frame(dt_lva)))

# ========================	
# save data 	
# ========================		
	
	save(dt_lva, file="data\\prepared\\LVA_prepared.Rdata")













		
