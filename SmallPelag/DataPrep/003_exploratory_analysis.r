# RCG BA subgroup work on Sampling Plan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019


# ==================
# Exploratory analysis of call data
# ==================
	
	rm(list=ls())
	
	library(data.table)	
	library(mapplots)
	library(shapefiles)
	
	load(file="data\\prepared\\data_compiled.Rdata")		

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
	

	# ===============
	# heatmaps
	# ===============
	
		# all gears
		windows(30,15); par(mfrow=c(1,2))
		heatmap_ices_rect_one_var(x = as.data.frame(dt1[sppName=="Clupea harengus"  & year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "her 2017", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
		heatmap_ices_rect_one_var(x = as.data.frame(dt1[sppName=="Clupea harengus"  & year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "her 2018", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\map_herring",".png", sep=""), type="png")

		windows(30,15); par(mfrow=c(1,2))
		heatmap_ices_rect_one_var(x = as.data.frame(dt1[sppName=="Sprattus sprattus"  & year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "spr 2017", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
		heatmap_ices_rect_one_var(x = as.data.frame(dt1[sppName=="Sprattus sprattus"  & year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "spr 2018", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\map_sprat",".png", sep=""), type="png")

		# per gear
			dt1$gear_simple<-substr(dt1$foCatEu6,1,3)
			dt1$gear_simple[is.na(dt1$gear_simple) | dt1$gear_simple %in% c("SB_","OTT","MIS","SDN","LLS","LLD","","LHP","FPN","FPO")]<-"OTHER_SB_OTT_MIS_SDN_LLS_LLD_LHP_FPN_FPO"
			dt1$gear_simple[is.na(dt1$gear_simple) | dt1$gear_simple %in% c("GNS","GTR")]<-"GNS_GTR"
			
			# heatmap per gear
	
			for (spp in c("Clupea harengus","Sprattus sprattus"))
			{
				for(gear in unique(c(dt1$gear_simple[dt1$sppName==spp], dt1$gear_simple[dt1$sppName==spp])))
				{
				windows(30,15); par(mfrow=c(1,2))
				print(gear)
				if (spp == "Clupea harengus") sppName = "her" else sppName = "spr"
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$gear_simple==gear   & year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,2017), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$gear_simple==gear  & year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,2018), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				title(main=gear, outer=T, line=-1)
				if (spp == "Clupea harengus") sppName = "herring" else sppName = "sprat"
				savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\map_", sppName,"_",gear,".png", sep=""), type="png")
			}
			}
			graphics.off()
			
			# heatmap per gearType
	
			for (spp in c("Clupea harengus","Sprattus sprattus"))
			{
				for(gear in unique(c(dt1$gearType)))
				{
				windows(30,15); par(mfrow=c(1,2))
				print(gear)
				if (spp == "Clupea harengus") sppName = "her" else sppName = "spr"
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$gearType==gear   & year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,2017), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName==spp & dt1$gearType==gear  & year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(sppName,2018), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				title(main=gear, outer=T, line=-1)
				if (spp == "Clupea harengus") sppName = "herring" else sppName = "sprat"
				savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\map_", sppName,"_",gear,".png", sep=""), type="png")
			}
			}
			graphics.off()			

			# heatmap per stock
	
				for(stock1 in unique(dt1[stock!="NULL",]$stock))
				{
				windows(30,15); par(mfrow=c(1,2))
				print(stock1)
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[stock==stock1 & year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(stock1,2017), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				heatmap_ices_rect_one_var(x = as.data.frame(dt1[stock==stock1 & year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = paste(stock1,2018), legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
				title(main=stock1, outer=T, line=-1)
				savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\map_", stock1,".png", sep=""), type="png")
			}
			}
			graphics.off()

			
	# ===============
	# barplots
	# ===============			

	
	library(xlsx)
	source("funs/func_barplot_var_by_one_var.r")	
	graph_all <- read.table("aux_files\\aux_RCG_BA_small_pelagics_Graphical_details.txt", sep="\t", stringsAsFactors=FALSE, header=T)
	colour_table<-read.table("aux_files\\aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")

	windows(10,7)
	for (year in 2017:2018)
		{
		 if (year == 2017) {df1<-dt1[dt1$year==2017,]}
		 if (year == 2018) {df1<-dt1[dt1$year==2018,]}
		for (spp in c("herring","sprat"))
			{
			if (spp == "herring") {df2<-df1[df1$sppName=="Clupea harengus",]}
			if (spp == "sprat") {df2<-df1[df1$sppName=="Sprattus sprattus",]}
			graph_det<-graph_all[graph_all$year==year & graph_all$Species==spp,]

			for (i in 1:nrow(graph_det))
			{	
			res<-barplot_var_by_one_var(x = as.data.frame(df2), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])))
			#res<-barplot_var_by_one_var(x = as.data.frame(SWE_V1_2018[SWE_V1_2018$sppName=="Sprattus sprattus",]), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])))
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}
		}
	}
	
		# species by year
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2017,]), Var = "landThousTon" , var1 = "sppName", tapply_type = graph_det$tapply_type[i], type_of_threshold = "main", value_of_threshold = 10, sorted=TRUE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_main_species_2017",".png", sep=""), type="png")
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2018,]), Var = "landThousTon" , var1 = "sppName", tapply_type = graph_det$tapply_type[i], type_of_threshold = "main", value_of_threshold = 10, sorted=TRUE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_main_species_2018",".png", sep=""), type="png")

		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2017,]), Var = "fishTripId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_Ntrips_vslLenCls_2017",".png", sep=""), type="png")
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2018,]), Var = "fishTripId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_Ntrips_vslLenCls_2018",".png", sep=""), type="png")

		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2017,]), Var = "vslId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_Nvessels_vslLenCls_2017",".png", sep=""), type="png")
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2018,]), Var = "vslId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_Nvessels_vslLenCls_2018",".png", sep=""), type="png")
		

		source("funs/func_barplot_var_by_two_var_stacked.r")	
		
		# lanCat stacked
		windows(30,15); par(mfrow=c(1,2))
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$year==2017,]),  Var = "landThousTon", var1 = "sppName", var2 = "landCat", tapply_type = "sum", proportion = FALSE, type_of_threshold = "main", value_of_threshold = 5, sorted = TRUE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2017")		
		par(new=T)
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$year==2018,]),  Var = "landThousTon", var1 = "sppName", var2 = "landCat", tapply_type = "sum", proportion = FALSE, type_of_threshold = "main", value_of_threshold = 5, sorted = TRUE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2018")		
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_sppName_landCat_2017_2018",".png", sep=""), type="png")


		# country stacked
		windows(30,15); par(mfrow=c(1,2))
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$year==2017,]),  Var = "landThousTon", var1 = "sppName", var2 = "vslFlgCtry", tapply_type = "sum", proportion = FALSE, type_of_threshold = "main", value_of_threshold = 5, sorted = FALSE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2017")		
		par(new=T)
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$year==2018,]),  Var = "landThousTon", var1 = "sppName", var2 = "vslFlgCtry", tapply_type = "sum", proportion = FALSE, type_of_threshold = "main", value_of_threshold = 5, sorted = FALSE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2018")		
		savePlot(filename = paste("outputs\\003_Exploratory_Analysis\\graph_sppName_vslFlgCtry_2017_2018",".png", sep=""), type="png")

		for (ctr in unique(dt1$vslFlgCtry))
		{
		ylimite=c(0, max(sum(dt1[dt1$year==2017 & vslFlgCtry==ctr,]$landThousTon), sum(dt1[dt1$year==2018 & vslFlgCtry==ctr,]$landThousTon)))
		windows(10,7)
		par(mfrow=c(1,2))
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2017 & vslFlgCtry==ctr,]), Var = "landThousTon" , var1 = "sppName", tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		#savePlot(filename = paste("outputs\\graph_Landings_vslFlgCtry_2017",".png", sep=""), type="png")
		par(new=T)
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$year==2018 & vslFlgCtry==ctr,]), Var = "landThousTon" , var1 = "sppName", tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		#savePlot(filename = paste("outputs\\graph_Landings_vslFlgCtry_2018",".png", sep=""), type="png")
		title(main=ctr, outer=T, line=-1)

	}

