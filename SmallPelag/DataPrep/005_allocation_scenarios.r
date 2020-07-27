# RCG BA subgroup work on Sampling Plan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019


# ==================
# Allocation scenarios
# ==================

	rm(list=ls())

	library(data.table)

	load(file="data\\prepared\\data_compiled_sampframe.Rdata")
	
	# must add stock
	
	 n_begin <- 589
	 
	 target_spp <- c("Clupea harengus","Sprattus sprattus")
	# No of trips for out of frame
	
	res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), list(vslFlgCtry, frame_vsl) ]
	res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), list(area, frame_vsl) ][order(area),]
	res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), list(area) ][order(area),]
	res$prop<-res$V1/sum(res$V1)
	res$n <- round(res$prop*n_begin)		
	 
	sum(res[frame_vsl==FALSE, ]$n) 
		# 63 samples with the following distribution
			res[frame_vsl==FALSE, ][order(area),]
	 
	# 
	n_update <- n_begin - sum(res[frame_vsl==FALSE, ]$n)	
	


# distribution by vslFlgCtry	
	
	# based on landings
		
		# of both species
		
			res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), vslFlgCtry]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res
			

			# with vessels size	
			res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), list(vslFlgCtry, vslLenCls)]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res			
		
		# of herring
			
			res<-dt1[sppName %in% "Clupea harengus" & year == 2017, sum(landWt), vslFlgCtry]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res

		# of sprat
			
			res<-dt1[sppName %in% "Sprattus sprattus" & year == 2017, sum(landWt), vslFlgCtry]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res

	# based on Trips

			res<-dt1[sppName %in% target_spp & year == 2017, length(unique(fishTripId)), vslFlgCtry]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res	

# distribution by area	
	
	# based on landings
		
		# of both species
		
			res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), area][order(area),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res


		
		# of herring
			
			res<-dt1[sppName %in% "Clupea harengus" & year == 2017, sum(landWt), area] [order(area),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res

			
			
		# of sprat
			
			res<-dt1[sppName %in% "Sprattus sprattus" & year == 2017, sum(landWt), area] [order(area),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res

			
	# based on Trips

			res<-dt1[sppName %in% target_spp & year == 2017, length(unique(fishTripId)), area] [order(area),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res				
	
	
# distribution by vslLenCls	
	
	# based on landings
		
		# of both species
		
			res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), vslLenCls][order(vslLenCls),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res			

			
	# based on Trips

			res<-dt1[sppName %in% target_spp & year == 2017, length(unique(fishTripId)), vslLenCls][order(vslLenCls),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res	
			


	
# distribution by stock
	#
	
	# based on landings
		
		# of both species
		
			res<-dt1[sppName %in% target_spp & year == 2017, sum(landWt), stock][order(stock),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res			
		write.csv2(res, file = "x5.csv")	

	

			
	# based on Trips

			res<-dt1[sppName %in% target_spp & year == 2017, length(unique(fishTripId)), stock][order(stock),]
			res$prop<-res$V1/sum(res$V1)
			res$n <- round(res$prop*n_update)
			res	

			