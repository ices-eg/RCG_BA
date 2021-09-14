# =========================
# Dir structure and data prep
# =========================

# Nuno Prista, 2017-2020
# WKBIOPTIM 1-3 work adapted to small pelagic RDB datasets

	# wishlist
		# add versioning system to outputs
		# develop a check function
	
	rm(list=ls())
	
	library(data.table)
	
	# create directories
		dir.create("000_Auxiliary_Funs", showWarnings = FALSE)
		dir.create("000_Original_Data", showWarnings = FALSE)
		dir.create("000_Auxiliary_Tables", showWarnings = FALSE)
		dir.create("001_Prepared_Inputs", showWarnings = FALSE)
		dir.create("002_Exploratory_Analyses", showWarnings = FALSE)
		dir.create("002_Exploratory_Analyses/002_sampId_testsims", showWarnings = FALSE)
		dir.create("002_Exploratory_Analyses/004_sampId_barplots", showWarnings = FALSE)
		dir.create("003_Sim_Results", showWarnings = FALSE)
		dir.create("004_Sim_Analysis", showWarnings = FALSE)

	# copy data to 000_Original_Data	
	# copy col_names_conversion_table to 000_Auxiliary_Tables	
	
	# load data files
		stock<-"her.25-2932"
		load(paste("000_Original_Data/her.25-2932/RDB_CS_",stock,"_2009_2019_prepared_202102160702.Rdata"),sep="")

		
	# create data.table object
		ca_stock<-data.table(ca_stock)
	
	# move to preparation
		ca_stock$FishingActivityCategoryEuropeanLvl5<-as.character(hh_stock$FishingActivityCategoryEuropeanLvl5[match(ca_stock$CS_StationId,hh_stock$CS_StationId)])
		ca_stock$FishingActivityCategoryEuropeanLvl6<-as.character(hh_stock$FishingActivityCategoryEuropeanLvl6[match(ca_stock$CS_StationId,hh_stock$CS_StationId)])
		ca_stock$Gear<-as.character(hh_stock$Gear[match(ca_stock$CS_StationId,hh_stock$CS_StationId)])
		ca_stock$MeshSize<-as.character(hh_stock$MeshSize[match(ca_stock$CS_StationId,hh_stock$CS_StationId)])
		ca_stock$AgeQuality<-1
	
	# reads correspondence colnames data and colnames input sims
		ref_tab<-xlsx::read.xlsx("000_Auxiliary_Tables/col_names_conversion_table_RDB_RCG format.xlsx", sheetIndex=1, colClasses="character")
		ref_tab<-ref_tab[!is.na(ref_tab$CA_Standard) & ref_tab$CA_Standard!="",]


	# update column names to those used in script
		df0<-as.data.frame(ca_stock)
		colnames(df0)<-ref_tab$CA_Standard[match(tolower(colnames(df0)),tolower(ref_tab$RDB_RCG_Standard))]

	# removes columns not in accepted list
		df0<-df0[,-which(colnames(df0)=="not_defined")]
	
	# creates columns missing from CA standard
		for (i in ref_tab$CA_Standard[1:32])
			{if (!i %in% colnames(df0)) df0[i]<-"No info"} 

	# Column prep [project specific]
		# tweak on Sex
			table(df0$sex, useNA="al")
			df0$sex<-as.character(df0$sex)
			df0$sex[df0$sex=="" | df0$sex=="-" | is.na(df0$sex)]<-NA
			df0$sex<-factor(df0$sex, exclude=NULL)
			table(df0$sex, useNA="al")			
		# Tweak on maturity
			table(df0$matStage, useNA="al")
			df0$matStage<-as.character(df0$matStage)
			df0$matStage[df0$matStage=="0"]<-NA
			df0$matStage<-factor(df0$matStage, levels=c("1","2","3","4","5","6","7","8"))
			table(df0$matStage, useNA="al")
		# creates mature
			df0$mature<-NA
			df0$mature[!is.na(df0$matStage) & df0$matStage %in% c(0,1,2)]<-0
			df0$mature[!is.na(df0$matStage) & !df0$matStage %in% c(0,1,2)]<-1 # CHECK with Carina
			df0$mature<-factor(df0$mature, levels=sort(unique(df0$mature)))
			table(df0$mature, useNA="al")
		# creates sampID [adapt to your case]		
			df0$sampId<-paste(df0$vslFlgCtry, df0$year, df0$sampId, sep="_")
			ls1<-split(df0, df0$sampId)
			ls2<-lapply(ls1, function(x){x$indivId<-paste(x$sampId, 1:nrow(x),sep="_"); x})
			df0<-do.call("rbind", ls2)
			rownames(df0)<-NULL
			# QCA: should yield TRUE
				sum(duplicated(df0$indivId))==0
	
	# Save prepared data	
			df00<-df0[df0$useInSims=="YES" & df0$assumedSamplingMethod=="SRSWOR",]
			save(df00, file=paste("001_Prepared_Inputs/Input_data_",stock,"_SRSWOR.Rdata",sep=""))		
			df00<-df0[df0$useInSims=="YES" & df0$assumedSamplingMethod=="STSRSWOR",]
			save(df00, file=paste("001_Prepared_Inputs/Input_data_",stock,"_STSRSWOR.Rdata",sep=""))		



