# ====================
# Small Pelagic Simulation Script - Subgroup 1
# ====================

   
   rm(list=ls())		
    
	# read packages [not sure all are used but...]
	library(foreign) 
    library(reshape2)
    library(data.table)
	library(xlsx) 
	# library(readxl) # use instead of library(xlsx) if you run into java issues

	# load population data [found in sharepoint]
		# data call format extended with haulId [can also use fishingDateId]
		# some additional useful variables were added a posteriori
		# both in-frame and out-of-frame vessels present
					
    filename<-"testData.Rdata"
	load(filename)

	# load vector of vessels in frame
    filename<-"target_list.Rdata"
	load(filename)

	ls()
	
	# wishlist
		# fishdayId vs. haulId
		# POL: add possibility of different vessel numbers per week
		# figure with 3 maps
			# samples obtained
			# fishing of the vessel list
			# fishing of the fleet
		# add simulation loop, outputs, 95% confidence intervals
		# DNK: add possibility of taking 1 sample from all trips of the vessel list
		# check if possibility of % proportion in sample, and verything processed.

	# other
		# Code produced should
		# allow simulation of next trip / next trip*next week, all trips in  week and so on
		# allow simulation last haul vs 1st haul, all hauls in trip
		# allow simulation of refusal rates (at vessel level, if possible trip level)
		# visualize the impacts of sampling alternatives in terms of 
		# workload: number of samples obtained / number of fish
		# coverage:
		# species (sprat vs herring)
		# long vs short trips
		# vessel size
		# subdivisions
		# rectangles
		# cooperative vs non-cooperative vessels
		# vessel in vessel-list vs non-vessel list
		# allow future integration between countries (to look at regional outputs)



    # ======================
	# example of simulation
    # ======================
			
			set.seed(123)
			
			# test dataset
			dt0 <- testData
	
            #n vsl vessels per week, week as strata
				n_vessels = 5
					# quota sampling 
						# if FALSE - contacts n_vessels (some many not be fishing)
						# if TRUE - contacts vessels until n_vessels are found to be fishing
						fill_quota=FALSE                                
            
			# selection of the vessels
                list_weeks<-unique(dt0$depWeek)[order(unique(dt0$depWeek))]
                out<-data.frame()
                for (week in list_weeks)
                    {
                    if(fill_quota==FALSE){selectedVsl<-sample(target_list, size=n_vessels, replace=F)}
                    if(fill_quota==TRUE){selectedVsl<-sample(target_list, size=length(target_list), replace=F)}
                    out<-rbind(out, data.frame(week, vslId=selectedVsl))
                    }
                out$weektrip<-out$week+1   
           
		   table(out$vslId, out$week)[,1:12]
		   
            # selection of the trips
                # take first trip departing next week
                    for (i in 1:nrow(out))
                        {
                        out$fishTripId[i]<-unique(dt0[depWeek==out$weektrip[i] & vslId==out$vslId[i],]$fishTripId)[1]
                        #out$haulId[i]<-unique(dt0[fishTripId==out$fishTripId[i],]$haulId)[1]
                        }
                    if(fill_quota==TRUE) # takes a max of n_vessels
                    {
                        ls1<-split(out,out$week)
                        ls2<-lapply(ls1, function(x){
                            x[!is.na(x$fishTripId),][1:min(n_vessels,sum(!is.na(x$fishTripId))),]})
                        out<-do.call(rbind,ls2)
                        out<-out[!is.na(out$week),]    
                    }
					
            # pulling original data [choose one of the alternatives below]
             

				# alternative 1: ask only for the dominant of species in a target list
                    target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','haulId','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]                    
						selected_sample<-dt0agg[fishTripId %in% out$fishTripId,][sppName %in% target_spp2,list(sppName=sppName[which(landWt==max(landWt))]), list(haulId)]
						dt_sample <- dt0agg[paste(haulId, sppName) %in% paste(selected_sample$haulId, selected_sample$sppName),]
						# solves issues of no dominance (randomly samples)
						aux<-tapply(dt_sample$sppName, dt_sample$haulId, function(x){length(unique(x))})
						dt_sample$prob<-dt_sample$haulId %in% names(aux[aux>1])
						ls1<-split(dt_sample, dt_sample$haulId)
						ls2<-lapply(ls1, function(x){
													if (all(x$prob==TRUE)) {x<-x[sample(1:nrow(x), size=1),] } else {x}
													})
						dt_sample<-rbindlist(ls2)									
							# QCA: must yield TRUE if problems solved
								sum(tapply(dt_sample$sppName, dt_sample$haulId, function(x){length(unique(x))})>1)==0
						dt_sample$prob<-NULL
									
							# NOTE: there is a difference between fishTripId present in out and dt_sample 
							#		that is due to vessels not having fished target species
								length(unique(out$fishTripId[!is.na(out$fishTripId)]))
								length(unique(dt_sample$fishTripId))

								
				# alternative 2: Ask for all species in the list if they are present
					target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','haulId','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
						dt_sample <- dt0agg[fishTripId %in% out$fishTripId & sppName %in% target_spp2, ]

				# alternative 3: Ask for all species in the list if they are present in a significant proportion across target_spp2 (e.g., 0.3)
					# choose the significant proportion (means: spp will only be found in sample if %weight > sign_prop)
					sign_prop<-0.3
					target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','haulId','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
						dt_sample <- dt0agg[fishTripId %in% out$fishTripId & sppName %in% target_spp2, ]
							# trimming down when rare
								dim(dt_sample)
								aux <- merge(dt_sample[,list(prop_her = landWt[sppName=="Clupea harengus"]/sum(landWt)), list(haulId)],dt_sample[,list(prop_spr = landWt[sppName=="Sprattus sprattus"]/sum(landWt)), list(haulId)], by="haulId",all=T)
								aux$prop_her[is.na(aux$prop_her)]<-0; aux$prop_spr[is.na(aux$prop_spr)]<-0
								dt_sample <- dt_sample[paste(dt_sample$haulId,dt_sample$sppName) %in% paste(aux$haulId, "Clupea harengus")[aux$prop_her>sign_prop] | 
														paste(dt_sample$haulId,dt_sample$sppName) %in% paste(aux$haulId, "Sprattus sprattus")[aux$prop_spr>sign_prop],]
								dim(dt_sample)
				
			
			   # alternative 4: ask for random species in the target list 
				#ATT: check if data.table implementation is well done
                    target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','haulId','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg <- dt0; dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
                    #selected_sample <- dt0agg[haulId %in% out$haulId, list(sppName=sample(target_spp2,1)), list(haulId)]
					dt_sample <- dt0agg[fishTripId %in% out$fishTripId & sppName %in% target_spp2, ]
					ls1 <- split(dt_sample, dt_sample$haulId)
					ls2 <- lapply(ls1, function(x){
									if (nrow(x)>1) {x<-x[sample(1:nrow(x), size=1),] } else {x}
									})					
					dt_sample<-rbindlist(ls2)
					
				# alternative 5?: feel free to suggest	


			#should yield TRUE
			  sum(!dt_sample$fishTripId %in% out$fishTripId[!is.na(out$fishTripI)])==0
			
			# adds call week
				dt_sample$callWeek<-dt_sample$depWeek-1
     

	 
					# RESULT: No successful vessels per calling week
					dt_sample[, list(n_vsl=length(unique(vslId))), callWeek]
					dt_sample[area %in% c("27.3.d.24","27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28.2","27.3.d.29"), list(n_vsl=length(unique(vslId))), callWeek]
		
			# adds expected weight (wtInBox) and number (nInBox) of fish in box
					
					boxWt = 4	# in kg
					
					samples_her<-dt0agg[vslId %in% target_list, list(landWt=landWt), list(haulId,sppName)][, list(wtInBox = sum(landWt[sppName=="Clupea harengus"])/sum(landWt)*boxWt), haulId]
					samples_her$sppName<-"Clupea harengus"
					assumed_herWeight<-0.024 # in kg
					samples_her$nInBox<-round(samples_her$wtInBox/assumed_herWeight)
					
					samples_spr<-dt0agg[vslId %in% target_list, list(landWt=landWt), list(haulId,sppName)][, list(wtInBox = sum(landWt[sppName=="Sprattus sprattus"])/sum(landWt)*boxWt), haulId]
					samples_spr$sppName<-"Sprattus sprattus"
					assumed_sprWeight<-0.008 # in kg
					samples_spr$nInBox<-round(samples_spr$wtInBox/assumed_sprWeight)						

					dt_sample$wtInBox<-NA
					dt_sample$wtInBox[dt_sample$sppName=="Clupea harengus"]<-samples_her$wtInBox[match(paste(dt_sample[dt_sample$sppName=="Clupea harengus",]$haulId, dt_sample[dt_sample$sppName=="Clupea harengus",]$sppName), paste(samples_her$haulId, samples_her$sppName))]
					dt_sample$wtInBox[dt_sample$sppName=="Sprattus sprattus"]<-samples_spr$wtInBox[match(paste(dt_sample[dt_sample$sppName=="Sprattus sprattus",]$haulId, dt_sample[dt_sample$sppName=="Sprattus sprattus",]$sppName), paste(samples_spr$haulId, samples_spr$sppName))]
				
					dt_sample$nInBox<-NA
					dt_sample$nInBox[dt_sample$sppName=="Clupea harengus"]<-samples_her$nInBox[match(paste(dt_sample[dt_sample$sppName=="Clupea harengus",]$haulId, dt_sample[dt_sample$sppName=="Clupea harengus",]$sppName), paste(samples_her$haulId, samples_her$sppName))]
					dt_sample$nInBox[dt_sample$sppName=="Sprattus sprattus"]<-samples_spr$nInBox[match(paste(dt_sample[dt_sample$sppName=="Sprattus sprattus",]$haulId, dt_sample[dt_sample$sppName=="Sprattus sprattus",]$sppName), paste(samples_spr$haulId, samples_spr$sppName))]

			# adds expected and number of fish sampled from box (nInBoxSampled)
					
					dt_sample$nInBoxSampled<-dt_sample$nInBox
					dt_sample$nInBoxSampled[dt_sample$nInBoxSampled>50]<-50
			
			# creates useful object (restricts to target area)
				dt_sample2<-droplevels(dt_sample[dt_sample$area %in% c("27.3.d.24","27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28.2","27.3.d.29"),])
					
				# RESULT: No samples per area, quarter and spp
					#table(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName)
					table(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName)
					tapply(dt_sample2$haulId, list(dt_sample2$area, dt_sample2$landQuarter), function(x)length(unique(x)))
					
				# RESULT: No trips per area, quarter and spp
					#tapply(dt_sample$fishTripId, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), function(x) length(unique(x)))
					tapply(dt_sample2$fishTripId, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), function(x) length(unique(x)))

				# RESULT: No vessels per area, quarter and spp
					#tapply(dt_sample$vslId, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), function(x) length(unique(x)))
					tapply(dt_sample2$vslId, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), function(x) length(unique(x)))

				# RESULT: No fish potentially available per area, quarter and spp				
					#tapply(dt_sample$nInBox, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), sum)
					tapply(dt_sample2$nInBox, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum)
					apply(tapply(dt_sample2$nInBox, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum, na.rm=T), c(1,3), sum, na.rm=T)
				
				# RESULT: No fish potentially sampled per area, quarter and spp				
					#tapply(dt_sample$nInBoxSampled, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), sum)
					tapply(dt_sample2$nInBoxSampled, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum)
					apply(tapply(dt_sample2$nInBoxSampled, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum, na.rm=T), c(1,3), sum, na.rm=T)

			
			# subsamples (2 per subdiv within trip sampled)
				
				hauls_per_area = 2
				
				ls1 <- split(dt_sample, dt_sample$fishTripId)
				ls2 <- lapply(ls1, function(x, subsamp1 = hauls_per_area){
									y1 <- split(x, x$area)
									y2 <- lapply(y1, function(z, subsamp2 = subsamp1)
												{
												hauls<-unique(z$haulId)
												# subsamples hauls if n hauls > subsamp2
												if (length(hauls)>subsamp2) z<-z[z$haulId %in% sample(hauls, size = subsamp2),]
												z
												})
									x <- rbindlist(y2)
									x									
				})
				dt_sample_subsampled<-rbindlist(ls2)

			# creates useful object (restricts to target area)
				dt_sample_subsampled2<-droplevels(dt_sample_subsampled[dt_sample_subsampled$area %in% c("27.3.d.24","27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28.2","27.3.d.29"),])

				# RESULT: No samples per area, quarter and spp
					#table(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName)
					table(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName)
					
				# RESULT: No trips per area, quarter and spp
					#tapply(dt_sample$fishTripId, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), function(x) length(unique(x)))
					tapply(dt_sample_subsampled2$fishTripId, list(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName), function(x) length(unique(x)))

				# RESULT: No vessels per area, quarter and spp
					#tapply(dt_sample$vslId, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), function(x) length(unique(x)))
					tapply(dt_sample_subsampled2$vslId, list(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName), function(x) length(unique(x)))

				# RESULT: No fish potentially available per area, quarter and spp				
					#tapply(dt_sample$nInBox, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), sum)
					tapply(dt_sample_subsampled2$nInBox, list(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName), sum)
					apply(tapply(dt_sample_subsampled2$nInBox, list(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName), sum, na.rm=T), c(1,3), sum, na.rm=T)
				
				# RESULT: No fish potentially sampled per area, quarter and spp				
					#tapply(dt_sample$nInBoxSampled, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), sum)
					res<-tapply(dt_sample_subsampled2$nInBoxSampled, list(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName), sum); res
					apply(res, c(1,3), sum, na.rm=T)
					apply(res, c(3), sum, na.rm=T)
					sum(res, na.rm=T)
					
			
	# ===========================
	# comparing with minimum goals
	# ===========================
					
		# NOTE: file "MinSampleTargets.xlsx" found in sharepoint			
			spr<-read.xlsx ("MinSampleTargets.xlsx", sheetName="spr")
			her<-read.xlsx ("MinSampleTargets.xlsx", sheetName="her")
						
			## alternative for library(readxl): use if you run into java issues when loading library(xlsx)
			# spr<-as.data.frame(read_xlsx("MinSampleTargets.xlsx", sheet="spr"))			
			# her<-as.data.frame(read_xlsx("MinSampleTargets.xlsx", sheet="her"))			

				
		# prepares minimum goals 
			colnames(spr) <- c("area",1,2,3,4)
			spr$sppName <- "Sprattus sprattus"
			spr<-reshape2::melt(spr)
		
			colnames(her) <- c("area",1,2,3,4)
			her$sppName <- "Clupea harengus"
			her<-reshape2::melt(her)				
		
			dt_goals<-data.table(rbind(spr, her))
			colnames(dt_goals)[3:4]<-c("landQuarter", "nSamples")
		
			dt_goals$area<-as.character(dt_goals$area)
		
			# subsets to target region
			dt_goals2 <- dt_goals[dt_goals$area %in% dt_sample2$area,]
			
		# compares
			goals_samples2 <- tapply(dt_goals2$nSamples, list(dt_goals2$area, dt_goals2$landQuarter, dt_goals2$sppName), sum)
			goals_fish2 <- goals_samples2*50
				apply(goals_samples2, 3, sum)
				apply(goals_fish2, 3, sum)
		
			# ATT: generates missing quarters
				# ATT: testData only includes Q1 so results for other quarters will be miningless. I leave them in to reduce code editing when real Q1 to Q4 data is used as input
				if(any(c(1:4)%in% dt_sample_subsampled2$landQuarter))dt_sample_subsampled2$landQuarter<-factor(dt_sample_subsampled2$landQuarter, levels=1:4)
			
			res_samples2 <-  table(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName)
			res_fish2 <- tapply(dt_sample_subsampled2$nInBoxSampled, list(dt_sample_subsampled2$area, dt_sample_subsampled2$landQuarter, dt_sample_subsampled2$sppName), sum)
			res_fish2[is.na(res_fish2)]<-0
				apply(res_samples2, 3, sum)
				apply(res_fish2, 3, sum)
				
			# difference
			res_samples2-goals_samples2 
			res_samples2[res_samples2-goals_samples2<0]
			goals_samples2[res_samples2-goals_samples2<0]
				# restriction to meaningful results [use with testData only]
					(res_samples2-goals_samples2)[,1,] 
					res_samples2[res_samples2[,1,]-goals_samples2[,1,]<0]
					goals_samples2[res_samples2[,1,]-goals_samples2[,1,]<0]					
		
			
