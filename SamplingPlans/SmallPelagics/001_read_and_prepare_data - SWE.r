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
	file_swe <- "data\\original\\SWE_V1_06_06.Rdata"
 
# read data
	load(file_swe)
	dt_swe<-data.table(rbind(SWE_V1_2017,SWE_V1_2018)); rm(SWE_V1_2017,SWE_V1_2018)
	

# ==================
# check and fix data 
# ==================

	source("funs\\func_checkData.r")
		dt_swe<-data.table(checkData(x = as.data.frame(dt_swe)))

# ========================	
# save data 	
# ========================			
		
	save(dt_swe, file="data\\prepared\\SWE_prepared.Rdata")
		
		













		
