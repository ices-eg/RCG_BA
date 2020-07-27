# ========================
# downloads data from sharepoint
# ======================== 
 
source("funs/func_download_data_from_sharepoint.r")
 
# downloads rdb data from sharepoint 
	
	#sharepoint_address <- "https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/RCGIntersessionalWork/2018%20Meeting%20Docs/Data%20Analysis%20and%20Quality/Data"
	sharepoint_address <- "https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/Regional%20Coordination%20Meetings%20Baltic%202017/2019%20Meeting%20docs/06.%20Data/Data%20SMALL%20PELAGIC%20IN%20THE%20BALTIC/Combined%20Folder"
	files_to_download<-c("Denmark\\DNK_V1_01_05.Rdata","Sweden\\SWE_V1_06_06.Rdata","Estonia\\EST_2019_30_04.csv","Finland\\FIN.csv","Germany\\DEU_2019_05_08.csv","Latvia\\RCG SUB-GROUP_2017_2018_tables_LV.xlsx","Lithuania\\LTU_2019_29_04.csv","Poland\\POL_2019_15_04.csv")
	#files_to_download<-c("CL Landing 2009-2018.zip")
	#files_to_download<-c("CS TR Trip 2009-2018.zip","CS HH Station 2009-2018.zip","CS SL SpeciesList 2009-2018.zip","CS HL Length 2009-2018.zip", "CS CA SMAWL 2009-2018.zip","CE Effort 2009-2018.zip", "CL Landing 2009-2018.zip")
	dir_download_browser<-"\\\\storage-lk.slu.se/home$/nupr0001/Downloads"
	
	#for (i in 1:length(files_to_download)){if(file.exists(paste(dir_download_browser,files_to_download[i],sep="/"))) file.remove(paste(dir_download_browser,files_to_download[i],sep="/"))}

	download_data_from_sharepoint (sharepoint_address = sharepoint_address, filename_vector = files_to_download, dir_download_browser = dir_download_browser, dir_download_target = "data", unzip=FALSE, delete_zip=FALSE)
