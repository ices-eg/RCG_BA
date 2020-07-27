# RCG BA subgroup work on Sampling Plan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019


# ==================
# Analysis of Sampling Frame
# ==================
	
	rm(list=ls())
	
	library(data.table)	
	library(mapplots)
	library(shapefiles)
	
	load(file="data\\prepared\\data_compiled.Rdata")		

	source("funs\\func_heatmap_ices_rect_one_var.r")
	source("funs\\func_heatmap_ices_rect_one_var.r")
	source("funs\\func_legend_grid2.r")

	# read shapefiles	
	map_fao_areas<-list("shp" = read.shp("aux_files\\shapefiles\\RCG_BA_2021_FAOareas.shp"), "shx" = read.shx("aux_files\\shapefiles\\RCG_BA_2021_FAOareas.shx"), "dbf"= read.dbf("aux_files\\shapefiles\\RCG_BA_2021_FAOareas.dbf"))
	map_ices_rect<-list("shp" = read.shp("aux_files\\shapefiles\\RCG_BA_2021_ICESrect.shp"), "shx" = read.shx("aux_files\\shapefiles\\RCG_BA_2021_ICESrect.shx"), "dbf"= read.dbf("aux_files\\shapefiles\\RCG_BA_2021_ICESrect.dbf"))
	# low resolution
	data_dir<-"aux_files\\shapefiles\\GSHHG\\gshhg-shp-2.3.7\\GSHHS_shp\\l"
	map_coast<-list("shp" = read.shp(paste(data_dir,"GSHHS_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"GSHHS_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"GSHHS_l_L1.dbf", sep="/")))
	data_dir<-"aux_files\\shapefiles\\GSHHG\\gshhg-shp-2.3.7\\WDBII_shp\\l"
	map_borders<-list("shp" = read.shp(paste(data_dir,"WDBII_border_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"WDBII_border_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"WDBII_border_l_L1.dbf", sep="/")))
	map_list<-list(map_ices_rect = map_ices_rect, map_fao_areas = map_fao_areas, map_ices_rect = map_ices_rect, map_coast = map_coast, map_borders = map_borders)


	
# Determination of Sampling Frame
	
	# restriction to target years [based on landing date]
		dt1<-droplevels(dt1[dt1$year %in% c(2017, 2018),])
	
	
	# identification of target vessels
		dt1$ID<-paste(dt1$year, dt1$vslId)
		a <- dt1[sppName %in% c("Clupea harengus","Sprattus sprattus"), list(target = sum(landWt)), list(year, vslId,ID)]
		b <- dt1[,list(all = sum(landWt)), list(year, vslId, ID)]
		res<-merge(b,a[,list(ID,target)] , by="ID", all.x=T)
		res$prop_target<-res$target/res$all
		
			# set criteria
				criteria <- 0.95
				x <- res[prop_target >= criteria, .N, list(year, vslId, ID)]$ID
				
		dt1$target_vsl<-FALSE	
		dt1$target_vsl[dt1$ID %in% x]<-TRUE	
	
	# identification of active vessels
		x <- dt1[gearType == "Active",.N, list(year, vslId, ID)]$ID
		dt1$active_vsl<-FALSE	
		dt1$active_vsl[dt1$ID %in% x]<-TRUE	
			# note: there are a few vessels (n = 23) that operate with both active and passive during the year
				# they represent about 0.5% of the weight - for simplicity, they were excluded from the frame
				prob_cases<-dt1[active_vsl==TRUE & target_vsl==TRUE, length(unique(gearType)), list(year, vslId)][V1>1,]$vslId
				sum(dt1[dt1$vslId %in% prob_cases,]$landWt)
				sum(dt1[dt1$vslId %in% dt1[dt1$target_vsl & dt1$active_vsl,]$vslId,]$landWt)
				sum(dt1[dt1$vslId %in% prob_cases & sppName %in% c("Clupea harengus","Sprattus sprattus"),]$landWt)
				sum(dt1[dt1$vslId %in% dt1[dt1$target_vsl & dt1$active_vsl & sppName %in% c("Clupea harengus","Sprattus sprattus"),]$vslId,]$landWt)
		
		dt1$active_vsl[dt1$vslId %in% prob_cases]<-FALSE
		
	# final identification of frame vessels
		dt1$frame_vsl <- FALSE
		dt1$frame_vsl[dt1$target_vsl & dt1$active_vsl] <- TRUE

			# note: there are still some vessels that fished less than 10000 of sprat and herring - these are removed	
			
			target_spp <- c("Clupea harengus","Sprattus sprattus")
			dt1[frame_vsl==TRUE & sppName %in% target_spp & year == 2017, sum(landWt), list(vslId)][V1<10000,]
			prob_cases <- dt1[frame_vsl==TRUE & sppName %in% target_spp & year == 2017, sum(landWt), list(vslId)][V1<10000,][V1<10000,] 

		dt1$frame_vsl[dt1$vslId %in% prob_cases]<-FALSE
		
		
		
# coverage of sampling frame
		
	# coverage (% landWt)
		a <- dt1[sppName %in% c("Clupea harengus","Sprattus sprattus"),] 
		prop.table(tapply(a$landWt, list(a$year, a$frame_vsl, a$sppName), sum),c(3,1))

	# coverage (% vslId)
		dt1[, length(unique(vslId)), list(year, frame_vsl)] 

	# coverage (% fishTripId)
		dt1[, length(unique(fishTripId)), list(year, frame_vsl)] 

	# coverage (% vslId*vslLenCls)
		a<-dt1[, length(unique(vslId)), list(year, frame_vsl, vslLenCls)] 
		tapply(a$V1, list(a$vslLenCls, a$frame_vsl, a$year), sum)
	
	
# exploration of sampling frame
		
		dt2<-droplevels(dt1[frame_vsl==TRUE,])
		
		# gearType
		dt2[,length(unique(vslId)),list(gearType, year)][order(year),]

		# No. Trips
		dt2[,length(unique(fishTripId)),list(gearType, year)][order(year),]
		
		# Landings
			# HER
			dt2[dt2$sppName=="Clupea harengus",sum(landWt),list(gearType, year)][order(year),]
			# SPR
			dt2[dt2$sppName=="Sprattus sprattus",sum(landWt),list(gearType, year)][order(year),]
		
		
		save(dt1, dt2, file = "data\\prepared\\data_compiled_sampframe.Rdata")
		
		
		write.csv2(dt2[year == 2017, list(Ntrips = length(unique(fishTripId)), Nvessels = length(unique(vslId))),vslFlgCtry], file = "x2.csv") 
		write.csv2(dt2[year == 2017 & sppName=="Clupea harengus", list(land = round(sum(landThousTon),1)),vslFlgCtry], file = "x3.csv") 
		write.csv2(dt2[year == 2017 & sppName=="Sprattus sprattus", list(land = round(sum(landThousTon),1)),vslFlgCtry], file = "x4.csv") 

		write.csv2(dt1[year == 2017 & sppName=="Clupea harengus", list(land = round(sum(landThousTon),1)),vslFlgCtry], file = "x3.csv") 
		write.csv2(dt1[year == 2017 & sppName=="Sprattus sprattus", list(land = round(sum(landThousTon),1)),vslFlgCtry], file = "x4.csv") 

		
		
	# indicators of the coverage of the sampling plan

		# heatmap per gearType
	
			for (spp in c("Clupea harengus","Sprattus sprattus"))
			{
				for(gear in unique(c(dt1$frame_vsl)))
				{
				windows(30,15); par(mfrow=c(1,2))
				print(gear)
				if (spp == "Clupea harengus") sppName = "her" else sppName = "spr"
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$frame_vsl==gear & year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,2017), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$frame_vsl==gear  & year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,2018), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				title(main=gear, outer=T, line=-1)
				if (spp == "Clupea harengus") sppName = "herring" else sppName = "sprat"
				savePlot(filename = paste("outputs\\004_Sampling_Frame_Analysis\\map_", sppName,"_",gear,".png", sep=""), type="png")
			}
			}
			graphics.off()			
			
		# heatmap per gearType (1 graph per year
	
			for (spp in c("Clupea harengus","Sprattus sprattus"))
			{
				for(ano in 2017:2018)
				{
				windows(30,15); par(mfrow=c(1,2))
				print(gear)
				if (spp == "Clupea harengus") sppName = "HER" else sppName = "SPR"
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$frame_vsl==TRUE & year == ano,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,"(In plan)"), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$frame_vsl==FALSE  & year == ano,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,"(Outside plan)"), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				title(main=ano, outer=T, line=-1)
				if (spp == "Clupea harengus") sppName = "herring" else sppName = "sprat"
				savePlot(filename = paste("outputs\\004_Sampling_Frame_Analysis\\map_", sppName,"_",ano,".png", sep=""), type="png")
			}
			}
			graphics.off()		
		
		
		# barplots

			#...to do...
		
	