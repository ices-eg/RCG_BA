# ====================
# Small Pelagic Simulation Script - Subgroup 1
# ====================

	# 2020-06-25: 
		# added vessel-level refusals [https://github.com/ices-eg/RCG_BA/issues/17]
		# added within haul selection methods: "all_hauls","first_haul","last_haul","srswor_haul" [https://github.com/ices-eg/RCG_BA/issues/18]
		# added results object [prepared for simulation] [https://github.com/ices-eg/RCG_BA/issues/14]
		# streamlined main settings: moved and highlighted in beginning of script
		# streamlined results - now only final results (if specified, subsampling per area is  applied ahead of final result)
		# fixed bug: eliminated nuisance vslId.x in dt_sample
  
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
		# POL: add possibility of different vessel numbers per week
		# figure with 3 maps
			# samples obtained
			# fishing of the vessel list
			# fishing of the fleet
		# add simulation loop, outputs, 95% confidence intervals
		# DNK: add possibility of taking 1 sample from all trips of the vessel list
		# check if possibility of % proportion in sample, and verything processed.
		# simulation fishdayId: can it be made more realistic with averaging over number of fishing operations and implementing the haulId method?

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


	# ==========================
	# user settings
	# ==========================	
	
		# withinTripSampUnit
			# this is what will be sampled withinTrip
			withinTripSampUnit <- "haulId" # alternative: "fishdayId"
			
		# withinTripSampMethod	
			withinTripSampMethod <- "all_hauls" # alternative: "first_haul", "last_haul", "srswor_haul"

		# subsampleHaulsPerSubdivision	
			subsampleHaulsPerArea<- TRUE # alternative: TRUE, FALSE
			nHaulsMaxPerArea<-2
			
		# refusals
			# the way refusals are incorporated is by removing vessels after sampling has happened
			# define: typeRefusals and refusals
				# if you choose "None" there is no further specification
				# if you choose "percent", adjust percent below [random vessels representing min x% will be highlighted in final results]
				# if you choose "vessels", select vessels below [specific vessels will be highlighted in final results]
				typeRefusals<-"percent" # alternatives: "none", "percent", "vessels"
				if (typeRefusals=="none") refusals<-"none"
				if (typeRefusals=="percent") refusals<-20
				if (typeRefusals=="vessels") refusals<-c("XYZ00441" "XYZ00743")
	
	# ==========================
	# ==========================	

			
	# updates refusal if typeRefusals=="percent"
		if (typeRefusals=="percent") refusals<-sample(target_list, size=ceiling(length(target_list)*refusals/100))
			
	# displays vessels that refused
		refusals

	# creates withinTripSampUnit column
		testData$withinTripSampUnit<-testData[[withinTripSampUnit]]
	
	# makes a few consistency checks
		agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
		sum(duplicated(data.frame(testData)[,agg_columns]))
		if (withinTripSampUnit=="haulId" & sum(duplicated(data.frame(testData)[,agg_columns])))stop("Probable error in data format")	
		agg_columns <- c('withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
		sum(duplicated(data.frame(testData)[,agg_columns]))
		if (withinTripSampUnit=="haulId" & sum(duplicated(data.frame(testData)[,agg_columns])))stop("Probable error in data format")	
	
			# tries to solve if withinTripSampUnit=="fishdayId"
				# if there are duplicates, withinTripSampUnit is likely logbookdayId and not fishdayId
				# if (withinTripSampUnit=="fishdayId")
					# {
					# testData$withinTripSampUnit<-paste(testData$fishdayId, testData$rect, testData$area, testData$foCatEu6) # thi is 
					# testData<-testData[, list(landWt=sum(landWt)),agg_columns]
					# sum(duplicated(data.frame(testData)[,agg_columns]))	
					# }
			# if(sum(duplicated(data.frame(testData)[,agg_columns]))!=0) stop("Probable error in data format")		
	

    # ======================
	# example of simulation
    # ======================
			
			set.seed(123)
			
			# test dataset
			dt0 <- testData
	
			# nsims [do not change, this script is configured only for 1 simulation)
				n_sims=1
	
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
                        #out$withinTripSampUnit[i]<-unique(dt0[fishTripId==out$fishTripId[i],]$withinTripSampUnit)[1]
                        }
                    if(fill_quota==TRUE) # takes a max of n_vessels
                    {
                        ls1<-split(out,out$week)
                        ls2<-lapply(ls1, function(x){
                            x[!is.na(x$fishTripId),][1:min(n_vessels,sum(!is.na(x$fishTripId))),]})
                        out<-do.call(rbind,ls2)
                        out<-out[!is.na(out$week),]    
                    }
				
			# implements haul selection
				aux<-unique(dt0[dt0$fishTripId %in% out$fishTripId,c("fishTripId", "withinTripSampUnit")][order(fishTripId,withinTripSampUnit),])
				if(withinTripSampMethod == "all_hauls") selected_haulId <- aux$withinTripSampUnit
				if(withinTripSampMethod == "first_haul") selected_haulId <- aux[,list(withinTripSampUnit=head(withinTripSampUnit,1)) ,fishTripId]$withinTripSampUnit
				if(withinTripSampMethod == "last_haul") selected_haulId <- aux[,list(withinTripSampUnit=tail(withinTripSampUnit,1)) ,fishTripId]$withinTripSampUnit
				if(withinTripSampMethod == "srswor_haul") selected_haulId <- aux[,list(withinTripSampUnit=sample(withinTripSampUnit,size=1)) ,fishTripId]$withinTripSampUnit
			
			
            # pulling original data according to the sampling alternative selected]

				# alternative 1a: ask for the dominant  species in haul
					# in brief, asks fishers to provide a clean sample with the dominant species in haul
					# known caveats: realistic only in single-spp high-dominance situations, will fail in more equitable distributions: a clean sample with only one species  is provided by fisher even if there are other species significantly present in the haul
					
					# run next lines
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]                    
						selected_sample<-dt0agg[withinTripSampUnit %in% selected_haulId,][,list(sppName=sppName[which(landWt==max(landWt))]), list(withinTripSampUnit)]
						dt_sample <- dt0agg[paste(withinTripSampUnit, sppName) %in% paste(selected_sample$withinTripSampUnit, selected_sample$sppName),]
						# solves issues of no dominance (randomly samples)
						aux<-tapply(dt_sample$sppName, dt_sample$withinTripSampUnit, function(x){length(unique(x))})
						dt_sample$prob<-dt_sample$withinTripSampUnit %in% names(aux[aux>1])
						ls1<-split(dt_sample, dt_sample$withinTripSampUnit)
						ls2<-lapply(ls1, function(x){
													if (all(x$prob==TRUE)) {x<-x[sample(1:nrow(x), size=1),] } else {x}
													})
						dt_sample<-rbindlist(ls2)									
							# QCA: must yield TRUE if problems solved
								sum(tapply(dt_sample$sppName, dt_sample$withinTripSampUnit, function(x){length(unique(x))})>1)==0
								dt_sample$prob<-NULL
				
				# alternative 1b: ask only for the dominant of species in a target list (target_spp2)
					# in brief, asks fishers to provide a clean sample with the dominant species within a target list. 
					# known caveats: Clean sample of a species within target_spp2 is provided even if species is a minority relative to other species that may have occurred and are not in target_spp2
		
					# user-defined: you need to define the target list you want to consider
						target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
					# run next lines
						agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
						dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]                    
						selected_sample<-dt0agg[haulId %in% selected_haulId,][sppName %in% target_spp2,list(sppName=sppName[which(landWt==max(landWt))]), list(withinTripSampUnit)]
						dt_sample <- dt0agg[paste(withinTripSampUnit, sppName) %in% paste(selected_sample$withinTripSampUnit, selected_sample$sppName),]
						# solves issues of no dominance (randomly samples)
						aux<-tapply(dt_sample$sppName, dt_sample$withinTripSampUnit, function(x){length(unique(x))})
						dt_sample$prob<-dt_sample$withinTripSampUnit %in% names(aux[aux>1])
						ls1<-split(dt_sample, dt_sample$withinTripSampUnit)
						ls2<-lapply(ls1, function(x){
													if (all(x$prob==TRUE)) {x<-x[sample(1:nrow(x), size=1),] } else {x}
													})
						dt_sample<-rbindlist(ls2)									
							# QCA: must yield TRUE if problems solved
								sum(tapply(dt_sample$sppName, dt_sample$withinTripSampUnit, function(x){length(unique(x))})>1)==0
						dt_sample$prob<-NULL
									
							# NOTE: there is a difference between fishTripId present in out and dt_sample 
							#		that is due to vessels not having fished target species
								length(unique(out$fishTripId[!is.na(out$fishTripId)]))
								length(unique(dt_sample$fishTripId))

								
				# alternative 2a: Ask for all species in the haul if they are present
					# in brief, asks fishers to provide a sample with all species in haul. Sample weight proportions will be proportional to weight proportions in haul (i.e., assumed fully representative in weight)
					# known caveats: realism issues. There is a (perhaps too strong but frequently useful) assumption involved in fishers being able to take a sample representative in weight. All species will come up in sample no matter how rare. 
					agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
						dt_sample <- dt0agg[haulId %in% selected_haulId, ]

				# alternative 2b: Ask for all species present in a target list (target_spp2)
					# in brief, asks fishers to provide a sample with species from a list present in haul. Sample weight proportions by species will be proportional to weight proportions of target species in haul (i.e., assumed representative in weight of target species)
					# known caveats: not realistic, only provided in case it is ever considered useful. Assumes fishers will subset a few species representatively to haul weights of those species which is difficult to conceive.
					target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
						dt_sample <- dt0agg[haulId %in% selected_haulId & sppName %in% target_spp2, ]
						dim(dt_sample)

				# alternative 3a: Ask for all species in the haul that are present in a significant proportion (e.g., 0.3)
					# in brief: assumes that if a species is >sign_prop than it will be present in the sample. Those present will be present in the exact weight proportions to each other as found in the haul.
					# known caveats: more realistic, but requires definition of sign_prop. sign_prop is also expressed in weight (volume might reflect better the selection process involved in taking a box)
					sign_prop<-0.3
					agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
						dt_sample <- dt0agg[haulId %in% selected_haulId, ]
							# trimming down when rare
								dim(dt_sample)
								aux<-tapply(dt_sample$landWt, list(dt_sample$withinTripSampUnit,dt_sample$sppName), sum); aux[is.na(aux)]<-0; aux<-prop.table(aux,1); aux<-aux>sign_prop
								aux<-reshape2::melt(aux); aux<-aux[aux[,3]==TRUE,]
								dt_sample <- dt_sample[paste(dt_sample$withinTripSampUnit,dt_sample$sppName) %in% paste(aux[,1],aux[,2]),]
								dim(dt_sample)

				# alternative 3b: Ask fishers for all species in a list (target_spp2) but only returns the ones with a significant proportion across target_spp2 (e.g., 0.3)
					# choose the significant proportion (means: spp will only be found in sample if %weight > sign_prop)
					# known caveats: same assumptions as 3a and less realist, particularly when spp in target_spp2 are a small proportion of haul.
					sign_prop<-0.3
					target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg<-dt0;dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
						dt_sample <- dt0agg[haulId %in% selected_haulId & sppName %in% target_spp2, ]
							# trimming down when rare
								dim(dt_sample)
								aux<-tapply(dt_sample$landWt, list(dt_sample$withinTripSampUnit,dt_sample$sppName), sum); aux[is.na(aux)]<-0; aux<-prop.table(aux,1); aux<-aux>sign_prop
								aux<-reshape2::melt(aux); aux<-aux[aux[,3]==TRUE,]
								dt_sample <- dt_sample[paste(dt_sample$withinTripSampUnit,dt_sample$sppName) %in% paste(aux[,1],aux[,2]),]
								dim(dt_sample)
				


			   # alternative 4a: ask for random species from the haul
                   # 
					agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
					dt0agg <- dt0; dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
					dt_sample <- dt0agg[haulId %in% selected_haulId, ]
					dim(dt_sample)
					ls1 <- split(dt_sample, dt_sample$withinTripSampUnit)
					ls2 <- lapply(ls1, function(x){
									if (nrow(x)>1) {x<-x[sample(1:nrow(x), size=1),] } else {x}
									})					
					dt_sample<-rbindlist(ls2)
					dim(dt_sample)
			
			   # alternative 4b: ask for random species in the target list (target_spp2)
                   #					
				   target_spp2 <- c("Sprattus sprattus", "Clupea harengus")
                    agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
                    dt0agg <- dt0; dt0agg<-dt0agg[, list(landWt=sum(landWt)),agg_columns]
                 	dt_sample <- dt0agg[haulId %in% selected_haulId & sppName %in% target_spp2, ]
					dim(dt_sample)
					ls1 <- split(dt_sample, dt_sample$withinTripSampUnit)
					ls2 <- lapply(ls1, function(x){
									if (nrow(x)>1) {x<-x[sample(1:nrow(x), size=1),] } else {x}
									})					
					dt_sample<-rbindlist(ls2)
					
				# alternative 5?: feel free to suggest	


			#should yield TRUE
			  sum(!dt_sample$fishTripId %in% out$fishTripId[!is.na(out$fishTripId)])==0
			
			# adds call week
				dt_sample$callWeek<-dt_sample$depWeek-1
     
			# highlight refusals
				dt_sample$refusal<-FALSE
				dt_sample$refusal[dt_sample$vslId %in% refusals]<-TRUE
	 
					# RESULT: No successful vessels per calling week
					dt_sample[, list(n_contact=n_vessels, n_vsl_fishing=length(unique(vslId)), n_vsl_final=length(unique(vslId[refusal==FALSE]))), callWeek]
					dt_sample[area %in% c("27.3.d.24","27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28.2","27.3.d.29"), list(n_contact=n_vessels, n_vsl_fishing=length(unique(vslId)), n_vsl_final=length(unique(vslId[refusal==FALSE]))), callWeek]
		
			# adds expected weight (wtInBox) and number (nInBox) of fish in box
					
					boxWt = 4	# in kg
					
					samples_her<-dt0agg[vslId %in% target_list, list(landWt=landWt), list(withinTripSampUnit,sppName)][, list(wtInBox = sum(landWt[sppName=="Clupea harengus"])/sum(landWt)*boxWt), withinTripSampUnit]
					samples_her$sppName<-"Clupea harengus"
					assumed_herWeight<-0.024 # in kg
					samples_her$nInBox<-round(samples_her$wtInBox/assumed_herWeight)
					
					samples_spr<-dt0agg[vslId %in% target_list, list(landWt=landWt), list(withinTripSampUnit,sppName)][, list(wtInBox = sum(landWt[sppName=="Sprattus sprattus"])/sum(landWt)*boxWt), withinTripSampUnit]
					samples_spr$sppName<-"Sprattus sprattus"
					assumed_sprWeight<-0.008 # in kg
					samples_spr$nInBox<-round(samples_spr$wtInBox/assumed_sprWeight)						

					dt_sample$wtInBox<-NA
					dt_sample$wtInBox[dt_sample$sppName=="Clupea harengus"]<-samples_her$wtInBox[match(paste(dt_sample[dt_sample$sppName=="Clupea harengus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Clupea harengus",]$sppName), paste(samples_her$withinTripSampUnit, samples_her$sppName))]
					dt_sample$wtInBox[dt_sample$sppName=="Sprattus sprattus"]<-samples_spr$wtInBox[match(paste(dt_sample[dt_sample$sppName=="Sprattus sprattus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Sprattus sprattus",]$sppName), paste(samples_spr$withinTripSampUnit, samples_spr$sppName))]
				
					dt_sample$nInBox<-NA
					dt_sample$nInBox[dt_sample$sppName=="Clupea harengus"]<-samples_her$nInBox[match(paste(dt_sample[dt_sample$sppName=="Clupea harengus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Clupea harengus",]$sppName), paste(samples_her$withinTripSampUnit, samples_her$sppName))]
					dt_sample$nInBox[dt_sample$sppName=="Sprattus sprattus"]<-samples_spr$nInBox[match(paste(dt_sample[dt_sample$sppName=="Sprattus sprattus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Sprattus sprattus",]$sppName), paste(samples_spr$withinTripSampUnit, samples_spr$sppName))]

			# adds expected and number of fish sampled from box (nInBoxSampled)
					
					dt_sample$nInBoxSampled<-dt_sample$nInBox
					dt_sample$nInBoxSampled[dt_sample$nInBoxSampled>50]<-50
			
			# creates useful object (restricts to target area & non-refusals)
				dt_sample2<-droplevels(dt_sample[dt_sample$area %in% c("27.3.d.24","27.3.d.25","27.3.d.26","27.3.d.27","27.3.d.28.2","27.3.d.29") & dt_sample$refusal==FALSE,])


			# subsamples hauls per subdiv within trip sampled
				
				if (subsampleHaulsPerArea)
				{
			
					ls1 <- split(dt_sample2, dt_sample2$fishTripId)
					ls2 <- lapply(ls1, function(x, subsamp1 = nHaulsMaxPerArea){
										y1 <- split(x, x$area)
										y2 <- lapply(y1, function(z, subsamp2 = subsamp1)
													{
													hauls<-unique(z$withinTripSampUnit)
													# subsamples hauls if n hauls > subsamp2
													if (length(hauls)>subsamp2) z<-z[z$withinTripSampUnit %in% sample(hauls, size = subsamp2),]
													z
													})
										x <- rbindlist(y2)
										x									
					})
					dt_sample2<-rbindlist(ls2)
				}
				
			# creates results object
				aux_resNames<-c("haulId_per_AreaQuarterSpp","haulId_per_RectQuarterSpp","fishTripId_per_AreaQuarterSpp","vslId_per_AreaQuarterSpp","fishInBoxTotal_per_AreaQuarterSpp","fishInBoxSampled_per_AreaQuarterSpp")
				
				res <- sapply(1:n_sims, function(x) NULL)
				for (i in 1:n_sims) {res[[i]]<-sapply(aux_resNames, function(x) NULL)}
				
				# RESULT1: No samples per area, quarter and spp
					#table(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName)
					res[[1]][["haulId_per_AreaQuarterSpp"]]<-table(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName)

				# RESULT1: No samples per area, quarter and spp
					#table(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName)
					res[[1]][["haulId_per_RectQuarterSpp"]]<-table(dt_sample2$rect, dt_sample2$landQuarter, dt_sample2$sppName)
					
				# RESULT2: No trips per area, quarter and spp
					#tapply(dt_sample$fishTripId, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), function(x) length(unique(x)))
					res[[1]][["fishTripId_per_AreaQuarterSpp"]]<-tapply(dt_sample2$fishTripId, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), function(x) length(unique(x)))

				# RESULT3: No vessels per area, quarter and spp
					#tapply(dt_sample$vslId, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), function(x) length(unique(x)))
					res[[1]][["vslId_per_AreaQuarterSpp"]]<-tapply(dt_sample2$vslId, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), function(x) length(unique(x)))

				# RESULT4: No fish potentially available per area, quarter and spp				
					#tapply(dt_sample$nInBox, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), sum)
					res[[1]][["fishInBoxTotal_per_AreaQuarterSpp"]]<-tapply(dt_sample2$nInBox, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum)
					#apply(tapply(dt_sample2$nInBox, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum, na.rm=T), c(1,3), sum, na.rm=T)
				
				# RESULT5: No fish potentially sampled per area, quarter and spp				
					#tapply(dt_sample$nInBoxSampled, list(dt_sample$area, dt_sample$landQuarter, dt_sample$sppName), sum)
					res[[1]][["fishInBoxSampled_per_AreaQuarterSpp"]]<-tapply(dt_sample2$nInBoxSampled, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum)
					#apply(tapply(dt_sample2$nInBoxSampled, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum, na.rm=T), c(1,3), sum, na.rm=T)


	# ===========================
	# Maps
	# ===========================	


					
			
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
				if(any(c(1:4)%in% dt_sample2$landQuarter))dt_sample2$landQuarter<-factor(dt_sample2$landQuarter, levels=1:4)
			
			res_samples2 <-  table(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName)
			res_fish2 <- tapply(dt_sample2$nInBoxSampled, list(dt_sample2$area, dt_sample2$landQuarter, dt_sample2$sppName), sum)
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
		
			
