# RCG ISSG Baltic Regional Sampling
	# 2021 Nuno Prista @SLU, Sweden

	# this script prepares small pelagic datasets for further analysis

	 	rm(list=ls())
		gc()
		library(data.table)

		# a useful function
		faz_cmToscm<-function(x){
			x[x-floor(x)>0.5]<-floor(x[x-floor(x)>0.5])+0.5
			x[x-floor(x)>0 & x-floor(x)<0.5]<-floor(x[x-floor(x)>0 & x-floor(x)<0.5])
			x
		}

		system.time(load("000_Original_Data/RDB_RCG_BA_CS_rcg_2009_2019_prepared_202102152200.Rdata"))

		# select the stock you want
			stock_code <- "spr.22-32"	
			#stock_code <- "her.25-2932"	
			#stock_code <- "her.3031"	
			#stock_code <- "her.28"	
		
		if(stock_code == "spr.22-32")
		{
				
			# subset species
				sl_stock<-sl_rcg[Species == "Sprattus sprattus",]
				hl_stock<-hl_rcg[CS_SpeciesListId %in% sl_stock$CS_SpeciesListId,]
				hh_stock<-hh_rcg[CS_StationId %in% unique(sl_stock$CS_StationId),]
				tr_stock<-tr_rcg[CS_TripId %in% unique(hh_stock$CS_TripId),]
				ca_stock<-ca_rcg[Species == "Sprattus sprattus",]
			# subset area
				target_areas<-c("27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28","27.3.d.28.2","27.3.d.29","27.3.d.32")
				target_gears<-c("OTM_SPF","PTM_SPF","OTB_SPF")
				target_trips<-hh_stock$CS_TripId[hh_stock$Area %in% target_areas & hh_stock$FishingActivityCategoryEuropeanLvl5 %in% target_gears]
				hh_stock<-hh_stock[Area %in% target_areas & FishingActivityCategoryEuropeanLvl5 %in% target_gears,]
				tr_stock<-tr_stock[CS_TripId %in% unique(hh_stock$CS_TripId),]
				sl_stock<-sl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				hl_stock<-hl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				ca_stock<-ca_stock[Area %in% target_areas & CS_TripId %in% target_trips,]

				hh_stock$Area[hh_stock$Area=="27.3.d.28.2"]<-"27.3.d.28"
				ca_stock$Area[ca_stock$Area=="27.3.d.28.2"]<-"27.3.d.28"
				
				hh_stock<-droplevels(hh_stock)
				ca_stock<-droplevels(ca_stock)
				
				table(ca_stock$Area)
				
		}

		if(stock_code == "her.25-2932")
		{
				
			# subset species
				sl_stock<-sl_rcg[Species == "Clupea harengus",]
				hl_stock<-hl_rcg[CS_SpeciesListId %in% sl_stock$CS_SpeciesListId,]
				hh_stock<-hh_rcg[CS_StationId %in% unique(sl_stock$CS_StationId),]
				tr_stock<-tr_rcg[CS_TripId %in% unique(hh_stock$CS_TripId),]
				ca_stock<-ca_rcg[Species == "Clupea harengus",]
			# subset area
				table(hh_stock$FishingActivityCategoryEuropeanLvl5,hh_stock$FlagCountry,useNA="al")
				
				target_areas<-c("27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28","27.3.d.28.2","27.3.d.29","27.3.d.32")
				gulf_riga_rects<-c("45H2", "45H3","45H4", "44H2","44H3","44H4", "43H2","43H3","43H4", "42H3")
				# hh_stock[Area %in% target_areas & StatisticalRectangle %in% gulf_riga_rects, .N, FlagCountry]
				# ca_stock[Area %in% target_areas & StatisticalRectangle %in% gulf_riga_rects, .N, FlagCountry]
				target_gears<-c("OTM_SPF","PTM_SPF","OTB_SPF")
				hh_stock<-hh_stock[Area %in% target_areas & !StatisticalRectangle %in% gulf_riga_rects & FishingActivityCategoryEuropeanLvl5 %in% target_gears,]
				tr_stock<-tr_stock[CS_TripId %in% unique(hh_stock$CS_TripId),]
				sl_stock<-sl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				hl_stock<-hl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				
				target_trips<-hh_stock$CS_TripId
				ca_stock<-ca_stock[Area %in% target_areas & !StatisticalRectangle %in% gulf_riga_rects & CS_TripId %in% target_trips,]
				
				hh_stock$Area[hh_stock$Area=="27.3.d.28"]<-"27.3.d.28.2"
				ca_stock$Area[ca_stock$Area=="27.3.d.28"]<-"27.3.d.28.2"
				hh_stock<-droplevels(hh_stock)
				ca_stock<-droplevels(ca_stock)

				table(ca_stock$Area,ca_stock$FlagCountry)
				
				}

		if(stock_code == "her.28")
		{
				
			# subset species
				sl_stock<-sl_rcg[Species == "Clupea harengus",]
				hl_stock<-hl_rcg[CS_SpeciesListId %in% sl_stock$CS_SpeciesListId,]
				hh_stock<-hh_rcg[CS_StationId %in% unique(sl_stock$CS_StationId),]
				tr_stock<-tr_rcg[CS_TripId %in% unique(hh_stock$CS_TripId),]
				ca_stock<-ca_rcg[Species == "Clupea harengus",]
			# subset area
				target_areas<-c("27.3.d.28","27.3.d.28.1")
				gulf_riga_rects<-c("45H2", "45H3","45H4", "44H2","44H3","44H4", "43H2","43H3","43H4", "42H3")
				# hh_stock[Area %in% target_areas & StatisticalRectangle %in% gulf_riga_rects, .N, FlagCountry]
				# ca_stock[Area %in% target_areas & StatisticalRectangle %in% gulf_riga_rects, .N, FlagCountry]
				target_gears<-c("OTM_SPF","PTM_SPF","OTB_SPF")
				hh_stock<-hh_stock[Area %in% target_areas & StatisticalRectangle %in% gulf_riga_rects & FishingActivityCategoryEuropeanLvl5 %in% target_gears,]
				tr_stock<-tr_stock[CS_TripId %in% unique(hh_stock$CS_TripId),]
				sl_stock<-sl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				hl_stock<-hl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				
				target_trips<-hh_stock$CS_TripId
				ca_stock<-ca_stock[(Area %in% target_areas  & StatisticalRectangle %in% gulf_riga_rects) & CS_TripId %in% target_trips,]
				
				hh_stock$Area[hh_stock$Area=="27.3.d.28"]<-"27.3.d.28.1"
				ca_stock$Area[ca_stock$Area=="27.3.d.28"]<-"27.3.d.28.1"
				
				hh_stock<-droplevels(hh_stock)
				ca_stock<-droplevels(ca_stock)
				
				table(ca_stock$Area,ca_stock$FlagCountry)
				
		}
		
		if(stock_code == "her.3031")
		{
				
			# subset species
				sl_stock<-sl_rcg[Species == "Clupea harengus",]
				hl_stock<-hl_rcg[CS_SpeciesListId %in% sl_stock$CS_SpeciesListId,]
				hh_stock<-hh_rcg[CS_StationId %in% unique(sl_stock$CS_StationId),]
				tr_stock<-tr_rcg[CS_TripId %in% unique(hh_stock$CS_TripId),]
				ca_stock<-ca_rcg[Species == "Clupea harengus",]
			# subset area and gears
				target_areas<-c("27.3.d.30","27.3.d.31")
				target_gears<-c("OTM_SPF","PTM_SPF","OTB_SPF")
				target_trips<-hh_stock$CS_TripId[hh_stock$Area %in% target_areas & hh_stock$FishingActivityCategoryEuropeanLvl5 %in% target_gears]
				hh_stock<-hh_stock[Area %in% target_areas & FishingActivityCategoryEuropeanLvl5 %in% target_gears,]
				tr_stock<-tr_stock[CS_TripId %in% unique(hh_stock$CS_TripId),]
				sl_stock<-sl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				hl_stock<-hl_stock[CS_StationId %in% unique(hh_stock$CS_StationId),]
				ca_stock<-ca_stock[Area %in% target_areas & CS_TripId %in% target_trips,]
				
				hh_stock<-droplevels(hh_stock)
				ca_stock<-droplevels(ca_stock)
				
				table(ca_stock$Area, ca_stock$FlagCountry)				
				
		}		


		# fix of length classes [LengthClass_cm2]
			# in CA
				ca_stock$LengthClass_cm2<-ca_stock$LengthClass_cm
				# mm to scm
				ca_stock$LengthClass_cm2[ca_stock$LengthCode=="mm"]<-faz_cmToscm(ca_stock$LengthClass_cm2[ca_stock$LengthCode=="mm"])
				# fix mm instead of scm
				ca_stock[LengthCode=="scm" & !((LengthClass_cm-floor(LengthClass_cm)) %in% c(0,0.5)),]$LengthClass_cm2<-faz_cmToscm(ca_stock[LengthCode=="scm" & !((LengthClass_cm-floor(LengthClass_cm)) %in% c(0,0.5)),]$LengthClass_cm)
				# should yield TRUE
				all((ca_stock$LengthClass_cm2-floor(ca_stock$LengthClass_cm2)) %in% c(0, 0.5))
			# in HL
				hl_stock$LengthClass_cm2<-hl_stock$LengthClass_cm
				# mm to scm
				hl_stock$LengthClass_cm2[hl_stock$LengthCode=="mm"]<-faz_cmToscm(hl_stock$LengthClass_cm2[hl_stock$LengthCode=="mm"])
				# fix mm instead of scm
				hl_stock[LengthCode=="scm" & !((LengthClass_cm-floor(LengthClass_cm)) %in% c(0,0.5)),]$LengthClass_cm2<-faz_cmToscm(hl_stock[LengthCode=="scm" & !((LengthClass_cm-floor(LengthClass_cm)) %in% c(0,0.5)),]$LengthClass_cm)
				# should yield TRUE
				all((hl_stock$LengthClass_cm2-floor(hl_stock$LengthClass_cm2)) %in% c(0, 0.5))

		# algorithm to associate HL n to CA samples (where possible)
			ca_stock$CS_LengthId_Probable<-NULL
			
			aux<-hl_stock
			dim(aux); aux<-merge(aux, hh_stock[,list(CS_StationId,Area,StatisticalRectangle)], by="CS_StationId", all.x=T); dim(aux)
	
			# pass 1 [both SamplingType with the ID]
	
			# builds ID with mandatory variables

				dim(aux); aux[, auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory, StatisticalRectangle, LengthClass_cm2),]; dim(aux)
				dim(ca_stock); ca_stock[, auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory, StatisticalRectangle, LengthClass_cm2),]; dim(ca_stock)

				aux1<-unique(aux[,list(CS_LengthId,CS_StationId,auxID, CS_TripId)])
				aux2<-aux1[,list(CS_LengthId_Probable=paste(CS_LengthId, collapse=",")), by=list(auxID)]	


			# assignment of CS_LengthId_Probable to CA data	
				dim(ca_stock); ca_stock<-merge(ca_stock, aux2, by="auxID", all.x=T); dim(ca_stock)	
			
				# some results on assignment:
					# % allocated
					 #sum(!is.na(ca_stock$CS_StationId_Probable))/nrow(ca_stock)*100
					 sum(!is.na(ca_stock$CS_LengthId_Probable))/nrow(ca_stock)*100
					# % allocated to single haul
					 #sum(!is.na(ca_stock$CS_StationId_Probable) & !grepl(ca_stock$CS_StationId_Probable, pat=","))/nrow(ca_stock)*100
					 sum(!is.na(ca_stock$CS_LengthId_Probable) & !grepl(ca_stock$CS_LengthId_Probable, pat=","))/nrow(ca_stock)*100
					
		# some sampling problems 
			# CA samples that do not appear in HL or have multiple HL records
				prob_trips<-unique(ca_stock[is.na(ca_stock$CS_LengthId_Probable) | grepl(ca_stock$CS_LengthId_Probable, pat=","),]$CS_TripId)
				
				ca_stock[,list(selected=sum(!CS_TripId %in% prob_trips),out=sum(CS_TripId %in% prob_trips)),list(FlagCountry)]
				ca_stock[,list(selected=sum(!CS_TripId %in% prob_trips),out=sum(CS_TripId %in% prob_trips)),list(FlagCountry, SamplingType)]
				
				# all fish in prob_trips are flagged for discarding	
				ca_stock$useInSims<-"YES"
				ca_stock$useInSims[ca_stock$CS_TripId %in% prob_trips]<-"NO"
		
	# adds some useful HL info to CA
		ca_stock$CS_SpeciesListId<-hl_stock$CS_SpeciesListId[match(ca_stock$CS_LengthId_Probable,hl_stock$CS_LengthId)]
		ca_stock$NoAtLengthInSample<-hl_stock$NoAtLengthInSample[match(ca_stock$CS_LengthId_Probable,hl_stock$CS_LengthId)]
		ca_stock$SLSamplingType<-hl_stock$SamplingType[match(ca_stock$CS_LengthId_Probable,hl_stock$CS_LengthId)]
		
	# compares CA sample size to HL sample size
		# additional samples to exclude when considering length frequency (because n>NoAtLengthInSample)
		ca_stock[,list(n=.N),list(FlagCountry,CS_LengthId_Probable, LengthClass, NoAtLengthInSample)][n>NoAtLengthInSample,]
		prob_samples<-unique(ca_stock[,list(n=.N),list(FlagCountry,CS_LengthId_Probable, CS_SpeciesListId, LengthClass, NoAtLengthInSample)][n>NoAtLengthInSample,]$CS_SpeciesListId)

		ca_stock$useInSims[ca_stock$CS_SpeciesListId %in% prob_samples]<-"NO"

	# looks for evidence of stratification
		# find the dominant classes in each trip 	
	
	# evidence of stratification is subsampling
		ls1<-split(ca_stock, ca_stock$CS_SpeciesListId)
		ls2<-lapply(ls1, function(x){
				data.frame(CS_SpeciesListId=x$CS_SpeciesListId[1],
								FlagCountry=x$FlagCountry[1],
								Year=x$Year[1],
								CASamplingType=x$SamplingType[1],
								SLSamplingType=x$SLSamplingType[1],
								hl_sampsize = sum(unique(x[,list(CS_LengthId_Probable,NoAtLengthInSample)])$NoAtLengthInSample),
									ca_sampsize = nrow(x)
										#swfscMisc:unifTest = ifelse(length(x$LengthClass_cm2)>10, uniform.test(hist(x$LengthClass_cm2, plot=F), B = 100)[[3]], NA)
										)
									})
		aux3<-data.table(do.call("rbind", ls2))
		
		# size match
			aux3[, list(total=.N, thesame=round(sum(CASamplingType==SLSamplingType)/.N,2)), ,list(FlagCountry)]
			aux3[, list(total=.N, likelyStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),FlagCountry]
			aux3[FlagCountry=="POL", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]
			aux3[FlagCountry=="LVA", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]
			aux3[FlagCountry=="DEU", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]
			aux3[FlagCountry=="FIN", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]
			aux3[FlagCountry=="SWE", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]
			aux3[FlagCountry=="EST", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]
			aux3[FlagCountry=="LTU", list(total=.N, couldBeStratified=round(sum(!hl_sampsize==ca_sampsize)/.N,2)),list(FlagCountry,Year,CASamplingType)][order(CASamplingType),]

			if (stock_code=="spr.22-32"){				
				# "spr.22-32"	
						   # FlagCountry total likelyStratified
						# 1:         LVA   357             0.79 -> stratified
						# 2:         SWE   574             0.14
						# 3:         FIN   100             0.90 -> stratified
						# 4:         EST   719             0.75 -> stratified
						# 5:         DEU   178             0.95 -> stratified
						# 6:         LTU   107             0.84 -> stratified
						# 7:         POL   408             0.80 -> stratified
						# 8:         DNK   165             0.84	-> stratified				

					# SWE only 2011 onwards
			

								aux3$assumedSamplingMethod<-"STSRSWOR"
								aux3$assumedSamplingMethod[aux3$FlagCountry=="SWE" & aux3$Year>=2011]<-"SRSWOR"
								prob_samples<-aux3[ca_sampsize>hl_sampsize,]$CS_SpeciesListId
								ca_stock$useInSims[ca_stock$CS_SpeciesListId %in% prob_samples]<-"NO"
								ca_stock$assumedSamplingMethod<-"STSRSWOR"
								ca_stock$assumedSamplingMethod[ca_stock$FlagCountry=="SWE" & ca_stock$Year>=2011]<-"SRSWOR"
								ca_stock[,list(totalFish=.N, useInSims=sum(useInSims=="YES"), percUseInSims=round(sum(useInSims=="YES")/.N,2)), list(FlagCountry)][order(FlagCountry),]
								}
				
			if (stock_code=="her.25-2932"){					
				# "her.25-2932"	
							# FlagCountry total likelyStratified
						# 1:         SWE   714             0.10
						# 2:         LVA   133             0.80 -> stratified
						# 3:         FIN   272             0.96 -> stratified
						# 4:         EST   663             0.01
						# 5:         DEU    40             0.02
						# 6:         LTU   138             0.64 -> stratified?
						# 7:         POL   496             0.51 -> stratified?
						# 8:         DNK   128             0.01
									
					# SWE, EST, DEU, DNK do not appear to stratify; SWE only 2011 onwards
					
								aux3$assumedSamplingMethod<-"STSRSWOR"
								aux3$assumedSamplingMethod[(aux3$FlagCountry=="SWE" & aux3$Year>=2011) | aux3$FlagCountry %in% c("EST","DEU","DNK")]<-"SRSWOR"
								prob_samples<-aux3[ca_sampsize>hl_sampsize,]$CS_SpeciesListId
								ca_stock$useInSims[ca_stock$CS_SpeciesListId %in% prob_samples]<-"NO"
								ca_stock$assumedSamplingMethod<-"STSRSWOR"
								ca_stock$assumedSamplingMethod[(ca_stock$FlagCountry=="SWE" & ca_stock$Year>=2011) | ca_stock$FlagCountry %in% c("EST","DEU","DNK")]<-"SRSWOR"
								ca_stock[,list(totalFish=.N, useInSims=sum(useInSims=="YES"), percUseInSims=round(sum(useInSims=="YES")/.N,2)), list(FlagCountry)][order(FlagCountry),]
								}					
					
			if (stock_code=="her.28"){					
				#stock_code <- "her.28"					
							# FlagCountry total likelyStratified
						# 1:         LVA   304             0.93 -> stratified
						# 2:         EST   177             0.05				
					# EST does not appear to stratify
				
				#stock_code <- "her.3031"	
						   # FlagCountry total likelyStratified
						# 1:         FIN   618             0.95 -> stratified
						# 2:         SWE     1             0.00
							
								aux3$assumedSamplingMethod<-"STSRSWOR"
								aux3$assumedSamplingMethod[aux3$FlagCountry %in% c("EST")]<-"SRSWOR"
								prob_samples<-aux3[ca_sampsize>hl_sampsize,]$CS_SpeciesListId
								ca_stock$useInSims[ca_stock$CS_SpeciesListId %in% prob_samples]<-"NO"
								ca_stock$assumedSamplingMethod<-"STSRSWOR"
								ca_stock$assumedSamplingMethod[ca_stock$FlagCountry %in% c("EST")]<-"SRSWOR"
								ca_stock[,list(totalFish=.N, useInSims=sum(useInSims=="YES"), percUseInSims=round(sum(useInSims=="YES")/.N,2)), list(FlagCountry)][order(FlagCountry),]
				}

			if (stock_code=="her.3031"){		
								aux3$assumedSamplingMethod<-"STSRSWOR"
								aux3$assumedSamplingMethod[aux3$FlagCountry %in% c("SWE")]<-"SRSWOR"
								prob_samples<-aux3[ca_sampsize>hl_sampsize,]$CS_SpeciesListId
								ca_stock$useInSims[ca_stock$CS_SpeciesListId %in% prob_samples]<-"NO"
								ca_stock$assumedSamplingMethod<-"STSRSWOR"
								ca_stock$assumedSamplingMethod[ca_stock$FlagCountry %in% c("SWE")]<-"SRSWOR"
								ca_stock[,list(totalFish=.N, useInSims=sum(useInSims=="YES"), percUseInSims=round(sum(useInSims=="YES")/.N,2)), list(FlagCountry)][order(FlagCountry),]
				}
	
	
		time_tag<-format(Sys.time(), "%Y%m%d%H%M")	
		year_start <- 2009	
		year_end <- 2019	
		output_dir<-paste("000_Original_Data/", stock_code, sep=""); dir.create(output_dir)
		save(tr_stock, hh_stock, sl_stock, hl_stock ,ca_stock, file_info_cs, file = paste(output_dir, paste("/RDB_CS",stock_code, year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))

