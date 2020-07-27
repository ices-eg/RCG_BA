# RCG BA subgroup work on Sampling PLan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019


library(data.table)		
		
# ========================
# reads in data
# ========================
 
 for(ctry in c("DEU","DNK","EST","FIN","LTU","LVA","POL","SWE")) 
	{
	load(paste("data\\prepared\\",ctry, "_prepared.Rdata", sep=""))
	}
	
	dt1<-rbind(dt_deu, dt_dnk, dt_est, dt_fin, dt_ltu, dt_lva, dt_pol, dt_swe)

# ========================	
# some additional variables
# ========================
	 
	# Year of landing
	dt1$year<-format(as.Date(dt1$landDate), format="%Y") 

	# Month of landing
	dt1$month<-factor(as.numeric(format(as.Date(dt1$landDate), format="%m")), levels=1:12) 
	
	# gearType: active or passive
	active_gears<-c('OTB','PTM','PTB','SDN','OTM','PS_','SB_','OTT')
	passive_gears<-unique(substring(dt1$foCatEu6,1,3))[!unique(substring(dt1$foCatEu6,1,3)) %in% active_gears]; passive_gears
	dt1$gearType<-NA
	dt1$gearType[substring(dt1$foCatEu6,1,3) %in% active_gears]<-"Active"
	dt1$gearType[!substring(dt1$foCatEu6,1,3) %in% active_gears]<-"Passive"

	
# ========================
# some additional checks and fixes on data
# ======================== 
 
	# issues on unique(vslLenCls) per trip		
		nrow(dt1[, length(unique(vslLenCls)), fishTripId][V1>1,])
	# issues on unique(vslLenCls) per vessel		
		nrow(dt1[, length(unique(vslLenCls)), list(year, vslId, gearType)][V1>1,])
		dt1[, length(unique(vslLenCls)), list(year, vslId, gearType)][V1>1,]
			# all passive - will be outside of sampling frame so ok.
	# issues on unique(depDate) per trip		
		nrow(dt1[, length(unique(depLoc)), fishTripId][V1>1,])
	# issues on unique(depLoc) per trip		
		nrow(dt1[, length(unique(depLoc)), fishTripId][V1>1,])
	# issues on unique(landDate) per trip		
		nrow(dt1[, length(unique(landDate)), fishTripId][V1>1,])
	# issues on unique(landLoc) per trip		
		nrow(dt1[, length(unique(landLoc)), fishTripId][V1>1,])
		dt1[, length(unique(landLoc)), list(fishTripId, gearType)][V1>1,]
	# slight fix of 4 nephrops trips (for consistency only)
		dt1$landLoc[dt1$fishTripId == "DNK20171121275"] <- "DKQSB"
		dt1$landLoc[dt1$fishTripId == "DNK20180816126"] <- "DKQSB"
		dt1$landLoc[dt1$fishTripId == "DNK20180816054"] <- "DKQSB"
		dt1$landLoc[dt1$fishTripId == "DNK20180815955"] <- "DKQSB"
		dt1[, length(unique(landLoc)), list(fishTripId, gearType)][V1>1,]
			# all remainder are passive - will be outside of sampling frame so ok.
	# issues on unique(sppCode) per sppName		
		nrow(dt1[, length(unique(sppCode)), sppName][V1>1,])
	# issues on unique(sppName) per sppCode		
		nrow(dt1[, length(unique(sppName)), sppCode][V1>1,])
			dt1$sppName[dt1$sppCode==127149]<-"Scophthalmus maximus"	
			dt1$sppName[dt1$sppCode==10194]<-"Actinopterygii"	
			nrow(dt1[, length(unique(sppName)), sppCode][V1>1,])
	# duplicates
			sum(duplicated(dt1))		
			dt1[duplicated(dt1) | duplicated(dt1, fromLast=T),]
			table(dt1[duplicated(dt1) | duplicated(dt1, fromLast=T),]$vslFlgCtry)
			# fix: elimination
			dt1<-dt1[!duplicated(dt1),]	
	# ID duplicates
		dt1[,.N, list(year, fishTripId, depDate, depLoc, landDate, landLoc, rect, area,foCatEu6, sppCode, sppName, landCat)][N>1,]
		# fix by aggregation
			dt1<-dt1[,list(landWt=sum(landWt)), list(year, vslFlgCtry,vslId,vslLenCls,fishTripId,depDate,depLoc,landDate,landLoc,rect,area,gearType,foCatEu6,sppCode,sppName,landCat)] 

	save(dt1, file="data_compiled.Rdata")		
			
			
# ==================
# Comparison with RDB data
# ==================			
			
	ref_data<-read.csv2("tot_rdb_baltic.csv")			
	ref_data<-rbind(ref_data, read.csv2("tot_rdb_nsea.csv"))			

	ref_data$Var1[ref_data$Var1 %in% c("27.3.d.28.1","27.3.d.28.2")]<-"27.3.d.28"	
	ref_data<-aggregate(value~Var4+Var2+Var1+Var3, data=ref_data, sum, na.rm=T)

	dt1$area[dt1$area %in% c("27.3.d.28.1","27.3.d.28.2")]<-"27.3.d.28"	
	datacall_data<-dt1[sppName %in% c("Clupea harengus","Sprattus sprattus"),sum(landWt), list(year, vslFlgCtry, area, sppName)][order(vslFlgCtry),]

	ref_data$ID<-paste(ref_data$Var4, ref_data$Var2, ref_data$Var1, ref_data$Var3)
	datacall_data$ID<-paste(datacall_data$year, datacall_data$vslFlgCtry, datacall_data$area, datacall_data$sppName)

	# aggregation year*area
	a<-merge(datacall_data, ref_data[c("ID","value")], by="ID", all.x=T)
	a$res<-round((a$V1-a$value)/a$value,2)
colnames(a)[colnames(a)=="V1"]<-"datacall"
colnames(a)[colnames(a)=="value"]<-"rdb"

	write.csv2(a, file="comparison_rdb_datacall_year_and_area.csv")
	
	# aggregation year
	a<-a[,list(V1=sum(V1), value=sum(value, na.rm=T)), list(vslFlgCtry, sppName, year)][order(vslFlgCtry),]
	a$res<-round((a$V1-a$value)/a$value,2)
colnames(a)[colnames(a)=="V1"]<-"datacall"
colnames(a)[colnames(a)=="value"]<-"rdb"
	write.csv2(a, file="comparison_rdb_datacall_year.csv")




# ==================
# Analysis
# ==================
		
		dt_deu$sppCode_asfis<-NULL

		dt1<-rbind(dt_swe, dt_dnk, dt_lva, dt_ltu, dt_est, dt_deu, dt_fin, dt_pol)
	

		
		
	library(mapplots)
	library(shapefiles)

	source("funs\\func_heatmap_ices_rect_one_var.r")
	source("funs\\func_legend_grid2.r")
	
	# read shapefiles	
	map_fao_areas<-list("shp" = read.shp("aux_files\\shapefiles\\RCG_BA_FAOareas.shp"), "shx" = read.shx("aux_files\\shapefiles\\RCG_BA_FAOareas.shx"), "dbf"= read.dbf("aux_files\\shapefiles\\RCG_BA_FAOareas.dbf"))
	map_ices_rect<-list("shp" = read.shp("aux_files\\shapefiles\\RCG_BA_ICESrect.shp"), "shx" = read.shx("aux_files\\shapefiles\\RCG_BA_ICESrect.shx"), "dbf"= read.dbf("aux_files\\shapefiles\\RCG_BA_ICESrect.dbf"))
	# low resolution
	data_dir<-"aux_files\\shapefiles\\GSHHG\\gshhg-shp-2.3.7\\GSHHS_shp\\l"
	map_coast<-list("shp" = read.shp(paste(data_dir,"GSHHS_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"GSHHS_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"GSHHS_l_L1.dbf", sep="/")))
	data_dir<-"aux_files\\shapefiles\\GSHHG\\gshhg-shp-2.3.7\\WDBII_shp\\l"
	map_borders<-list("shp" = read.shp(paste(data_dir,"WDBII_border_l_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"WDBII_border_l_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"WDBII_border_l_L1.dbf", sep="/")))
	map_list<-list(map_ices_rect = map_ices_rect, map_fao_areas = map_fao_areas, map_ices_rect = map_ices_rect, map_coast = map_coast, map_borders = map_borders)

	dt1$landThousTon<-dt1$landWt/10^6

	
	dt1$Month<-format(as.Date(dt1$landDate),"%m")
	dt1$Year<-format(as.Date(dt1$landDate),"%Y")
	
	#heatmap_ices_rect_one_var(x = as.data.frame(SWE_V1_2017), Var = "landWt", var1 = "rect", F=sum, map_title = "lol", legend_title = "lol", nbreaks = 6, xlim=c(5.5,30.5), ylim=c(50.25,65.5), legend_amp=1, map_list = map_list)
	windows(30,15); par(mfrow=c(1,2))
	heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName=="Clupea harengus"  & Year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "her 2017", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
	heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName=="Clupea harengus"  & Year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "her 2018", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
	savePlot(filename = paste("outputs\\map_herring",".png", sep=""), type="png")

	windows(30,15); par(mfrow=c(1,2))
	heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName=="Sprattus sprattus"  & Year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "spr 2017", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
	heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName=="Sprattus sprattus"  & Year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "spr 2018", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
	savePlot(filename = paste("outputs\\map_sprat",".png", sep=""), type="png")

	# heatmap herring gear
	dt1$gear_simple<-substr(dt1$foCatEu6,1,3)
	dt1$gear_simple<-substr(dt1$foCatEu6,1,3)
	
	#tweak: 
	dt1$gear_simple[is.na(dt1$gear_simple) | dt1$gear_simple %in% c("SB_","OTT","MIS","SDN","LLS","LLD","","LHP")]<-"OTHER_SB_OTT_MIS_SDN_LLS_LLD_LHP"
	for(gear in unique(c(dt1$gear_simple[dt1$sppName=="Clupea harengus"], dt1$gear_simple[dt1$sppName=="Clupea harengus"])))
	{
	windows(30,15); par(mfrow=c(1,2))
	print(gear)
	heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName=="Clupea harengus" & dt1$gear_simple==gear   & Year == 2017,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "her 2017", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
	heatmap_ices_rect_one_var(x = as.data.frame(dt1[dt1$sppName=="Clupea harengus" & dt1$gear_simple==gear  & Year == 2018,]), Var = "landThousTon", var1 = "rect", F=sum, map_title = "her 2018", legend_title = "1000Ton", nbreaks = 6, xlim=c(2.5,30.5), ylim=c(50.25,72.5), legend_amp=1, map_list = map_list)
	title(main=gear, outer=T, line=-1)
	savePlot(filename = paste("outputs\\map_herring_",gear,".png", sep=""), type="png")
	}
	
# barplots
	
	library(xlsx)
	source("funs/func_barplot_var_by_one_var.r")	
	graph_all <- read.table("RCG_BA_small_pelagics_Graphical_details.txt", sep="\t", stringsAsFactors=FALSE, header=T)
	colour_table<-read.table("aux_files\\aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")

	windows(10,7)
	for (year in 2017:2018)
		{
		 if (year == 2017) {df1<-dt1[dt1$Year==2017,]}
		 if (year == 2018) {df1<-dt1[dt1$Year==2018,]}
		for (spp in c("herring","sprat"))
			{
			if (spp == "herring") {df2<-df1[df1$sppName=="Clupea harengus",]}
			if (spp == "sprat") {df2<-df1[df1$sppName=="Sprattus sprattus",]}
			graph_det<-graph_all[graph_all$Year==year & graph_all$Species==spp,]

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
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2017,]), Var = "landThousTon" , var1 = "sppName", tapply_type = graph_det$tapply_type[i], type_of_threshold = "main", value_of_threshold = 10, sorted=TRUE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\graph_main_species_2017",".png", sep=""), type="png")
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2018,]), Var = "landThousTon" , var1 = "sppName", tapply_type = graph_det$tapply_type[i], type_of_threshold = "main", value_of_threshold = 10, sorted=TRUE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\graph_main_species_2018",".png", sep=""), type="png")

		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2017,]), Var = "fishTripId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\graph_Ntrips_vslLenCls_2017",".png", sep=""), type="png")
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2018,]), Var = "fishTripId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\graph_Ntrips_vslLenCls_2018",".png", sep=""), type="png")

		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2017,]), Var = "vslId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\graph_Nvessels_vslLenCls_2017",".png", sep=""), type="png")
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2018,]), Var = "vslId" , var1 = "vslLenCls", tapply_type = "length_unique", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		savePlot(filename = paste("outputs\\graph_Nvessels_vslLenCls_2018",".png", sep=""), type="png")
		

		source("funs/func_barplot_var_by_two_var_stacked.r")	
		
		# lanCat stacked
		windows(30,15); par(mfrow=c(1,2))
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$Year==2017,]),  Var = "landThousTon", var1 = "sppName", var2 = "landCat", tapply_type = "sum", proportion = FALSE, type_of_threshold = "main", value_of_threshold = 5, sorted = TRUE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2017")		
		par(new=T)
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$Year==2018,]),  Var = "landThousTon", var1 = "sppName", var2 = "landCat", tapply_type = "sum", proportion = FALSE, type_of_threshold = "main", value_of_threshold = 5, sorted = TRUE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2018")		
		savePlot(filename = paste("outputs\\graph_sppName_landCat_2017_2018",".png", sep=""), type="png")


		# country stacked
		windows(30,15); par(mfrow=c(1,2))
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$Year==2017,]),  Var = "landThousTon", var1 = "sppName", var2 = "vslFlgCtry", tapply_type = "sum", proportion = TRUE, type_of_threshold = "main", value_of_threshold = 5, sorted = FALSE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2017")		
		par(new=T)
		barplot_var_by_two_var_stacked(x = as.data.frame(dt1[dt1$Year==2018,]),  Var = "landThousTon", var1 = "sppName", var2 = "vslFlgCtry", tapply_type = "sum", proportion = TRUE, type_of_threshold = "main", value_of_threshold = 5, sorted = FALSE, graph_par  =list(oma = c(4,1,1,5), mai = c(1,1,0.7,1), ylab_line=4, col="colour4", cex.x=0.8), legend_par = "list(x=max(bar.coords)+diff(bar.coords)[1]/1.5, y=max(apply(t1,2,sum)), xjust=0, yjust=1, cex=0.7)", grouped=FALSE, title_root="2017")		
		savePlot(filename = paste("outputs\\graph_sppName_vslFlgCtry_2017_2018",".png", sep=""), type="png")

		for (ctr in unique(dt1$vslFlgCtry))
		{
		ylimite=c(0, max(sum(dt1[dt1$Year==2017 & vslFlgCtry==ctr,]$landThousTon), sum(dt1[dt1$Year==2018 & vslFlgCtry==ctr,]$landThousTon)))
		windows(10,7)
		par(mfrow=c(1,2))
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2017 & vslFlgCtry==ctr,]), Var = "landThousTon" , var1 = "sppName", tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		#savePlot(filename = paste("outputs\\graph_Landings_vslFlgCtry_2017",".png", sep=""), type="png")
		par(new=T)
		res<-barplot_var_by_one_var(x = as.data.frame(dt1[dt1$Year==2018 & vslFlgCtry==ctr,]), Var = "landThousTon" , var1 = "sppName", tapply_type = "sum", type_of_threshold = "NULL", value_of_threshold = 10, sorted=FALSE, graph_par = list(oma = c(6,1,1,1), mai = c(1,1,.5,.5), ylab_line=4, col="colour4", new=F))
		#savePlot(filename = paste("outputs\\graph_Landings_vslFlgCtry_2018",".png", sep=""), type="png")
		title(main=ctr, outer=T, line=-1)

	}






















		
