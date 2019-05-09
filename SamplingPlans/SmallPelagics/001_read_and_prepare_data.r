# RCG BA subgroup work on Sampling PLan for Small Pelagic 
	# Nuno Prista (SLU, Sweden), 2019
		# func "checkData" is a development of original work done by Alastair Pout (Marine Scotland, UK) during project fishpi2

# ========================
# reads in data
# ========================
 
 
rm(list=ls())
library(data.table)
library(fishPiCodes)	
library(foreign) 

 
 # read file names
 	file_deu <- "data\\original\\DEU_2019_05_08.csv"
	file_est <- "data\\original\\EST_2019_30_04.csv"
	file_fin <- "data\\original\\FIN.csv"
	file_ltu <- "data\\original\\LTU_2019_29_04.csv"
	file_pol <- "data\\original\\POL_2019_15_04.csv"
	file_lva <- "data\\original\\RCG SUB-GROUP_2017_2018_tables_LV.csv"
	file_dnk <- "data\\original\\DNK_V1_01_05.Rdata"
	file_swe <- "data\\original\\SWE_V1_29_04.Rdata"
 
# read data
	dt_deu<-fread(file_deu, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_est<-fread(file_est, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_fin<-fread(file_fin, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_ltu<-fread(file_ltu, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_pol<-fread(file_pol, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	dt_lva<-fread(file_lva, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL")
	load(file_dnk)
	dt_dnk<-as.data.table(dat9); rm(dat9)
	load(file_swe)
	dt_swe<-data.table(rbind(SWE_V1_2017,SWE_V1_2018)); rm(SWE_V1_2017,SWE_V1_2018)

	
	
# ========================
# checks data
# ========================
	
	
# ==================
# DEU	
# ==================

	source("func_checkData.r")				
	checkData (x = as.data.frame(dt_deu), ignore_stop=TRUE)

# minor: rename columns
	dt_deu$vslFlgCtry <- dt_deu$vslFglCtry; dt_deu$vslFglCtry<-NULL
	dt_deu$landWt <- dt_deu$LandWt; dt_deu$LandWt<-NULL
# minor issue with fishTripId
	# adding 00 to end of fishTripId to meet character number
	dt_deu$fishTripId<-paste(dt_deu$fishTripId,"00", sep="")
# minor: issues with sppCodes	
	dt_deu$sppCode[dt_deu$sppCode == 0 & dt_deu$sppName == "Prionotus spp"] <- 159569; 
	dt_deu$sppCode[dt_deu$sppCode == 0 & dt_deu$sppName == "Thunnini"] <- 125559; 
	dt_deu$sppName[dt_deu$sppName == "Thunnini"] <- "Scombridae"; 
# minor: issues with formating	
	dt_deu$landWt<-as.numeric(dt_deu$landWt)
	dt_deu$sppCode <- as.integer(dt_deu$sppCode)
# minor: issues with spp names and codes
	dt_deu$sppName[dt_deu$sppName=="Prionotus spp"] <- "Prionotus"
	dt_deu$sppName[dt_deu$sppName=="Trachurus spp"] <- "Trachurus"
	dt_deu$sppName[dt_deu$sppName=="Salmo spp"] <- "Salmo"
	dt_deu$sppName[dt_deu$sppName=="Stizostedion lucioperca"] <- "Sander lucioperca"
	dt_deu$sppName[dt_deu$sppName=="Abramis spp"] <- "Abramis"
# minor: issues with areas
	dt_deu$area[dt_deu$area=="27.3.a.n"]<-"27.3.a.20"
	
# to check
	# has vslLenCls "" or NA
		sum(is.na(dt_deu$vslLenCls)) # n = 10
	# has depDate "" or NA
		sum(is.na(dt_deu$depDate)) # n = 5
	# has foCAtEu6 "" or NA
		sum(is.na(dt_deu$foCatEu6)) # n = 10
		
	# issues with metiers [non existing in CL]
		dt_deu$foCatEu6<-as.character(dt_deu$foCatEu6)		
		dt_deu$foCatEu6[dt_deu$foCatEu6=="PTM_SPF_32-109_0_0"]<-"PTM_SPF_32-104_0_0" # 20 records
		dt_deu$foCatEu6[dt_deu$foCatEu6=="GNS_SPF_16-32_0_0"]<-"GNS_SPF_16-109_0_0" # 44 records
		dt_deu$foCatEu6[dt_deu$foCatEu6=="PTB_SPF_32-109_0_0"]<-"PTB_SPF_32-104_0_0" # 3 records
	
	# unknown ports [not in UNECE)

# landLoc "" or NA
	sum(is.na(dt_deu$landLoc)) # n = 5
# landLoc unknown	
	DEND1
	DEBR1
# depLoc "" or NA 	
	sum(is.na(dt_deu$depLoc)) # n = 5
# depLoc unknown		 
	DEND1
	DEBR1
# landCat unknown		 
	HCN # 12423
	UKN # 5078
	
# Days at sea greater than 20, for  1012 trips:	[all small scale]
# DEU20172348800 DEU20171499500 DEU20171581700 DEU20171809100 DEU20171877400 DEU20172793000 DEU20172793100 DEU20182202600 DEU20172358700 DEU20172366400 DEU20172366800 DEU20172374100 DEU20172374500 DEU20181221000 DEU20181237500 DEU20181245800 DEU20181307600 DEU20181374400 DEU20181379100 DEU20181410100 DEU20181424400 DEU20181426400 DEU20181436600 DEU20181438400 DEU20181439800 DEU20181493300 DEU20181494000 DEU20181530400 DEU20181542900 DEU20181662200 DEU20181668100 DEU20181681800 DEU20181728500 DEU20181753100 DEU20181760200 DEU20181770700 DEU20181851200 DEU20181858000 DEU20181875700 DEU20181884000 DEU20181996700 DEU20182198800 DEU20182220600 DEU20182221000 DEU20182228600 DEU20182229000 DEU20182550600 DEU20182586200 DEU20182630200 DEU20182633100 DEU20181137700 DEU20171171100 DEU20171335300 DEU20171356600 DEU20171357000 DEU20171365400 DEU20171377200 DEU20171482300 DEU20171482900 DEU20171485300 DEU20171486500 DEU20171504500 DEU20171519800 DEU20171572900 DEU20171582000 DEU20171587400 DEU20171624800 DEU20171624900 DEU20171640700 DEU20171722500 DEU20171768100 DEU20171797200 DEU20171797300 DEU20171797400 DEU20171875900 DEU20171877800 DEU20171886300 DEU20171911800 DEU20171917000 DEU20171929200 DEU20171942200 DEU20171974200 DEU20171974300 DEU20171977100 DEU20171977200 DEU20171978300 DEU20171984700 DEU20171984800 DEU20171985000 DEU20171994000 DEU20172010700 DEU20172011400 DEU20172107200 DEU20172136500 DEU20172136600 DEU20172136700 DEU20172217200 DEU20172241500 DEU20172357500 DEU20172366100 DEU20172366300 DEU20172366500 DEU20172366700 DEU20172367900 DEU20172368600 DEU20172373800 DEU20172374000 DEU20172374200 DEU20172374400 DEU20172741300 DEU20172741400 DEU20172793300 DEU20172797100 DEU20172825900 DEU20181039900 DEU20181137200 DEU20181160200 DEU20181220900 DEU20181318100 DEU20181320000 DEU20181320100 DEU20181332500 DEU20181378200 DEU20181419100 DEU20181482800 DEU20181507700 DEU20181512600 DEU20181531600 DEU20181542400 DEU20181591200 DEU20181691200 DEU20181853600 DEU20181883900 DEU20181996200 DEU20182204100 DEU20182709500 DEU20171041500 DEU20171054200 DEU20171054300 DEU20171073700 DEU20171073800 DEU20171105500 DEU20171105600 DEU20171107100 DEU20171121100 DEU20171121600 DEU20171134900 DEU20171177200 DEU20171205600 DEU20171264500 DEU20171283300 DEU20171292500 DEU20171295600 DEU20171344300 DEU20171345300 DEU20171366100 DEU20171366900 DEU20171374800 DEU20171374900 DEU20171391500 DEU20171440900 DEU20171441900 DEU20171442000 DEU20171485200 DEU20171537700 DEU20171574900 DEU20171575000 DEU20171576600 DEU20171591000 DEU20171623800 DEU20171640400 DEU20171640500 DEU20171788100 DEU20171842400 DEU20171916300 DEU20171916900 DEU20171942500 DEU20171978000 DEU20171984100 DEU20172000700 DEU20172010600 DEU20172136000 DEU20172136100 DEU20172139300 DEU20172139400 DEU20172359800 DEU20172359900 DEU20172360000 DEU20172740800 DEU20172913900 DEU20172914000 DEU20172914900 DEU20181379600 DEU20181379700 DEU20181039800 DEU20181616600 DEU20181618300 DEU20181618400 DEU20181618500 DEU20181619400 DEU20181040300 DEU20181060800 DEU20181060900 DEU20181061000 DEU20181061400 DEU20181061500 DEU20181817800 DEU20181932000 DEU20182006300 DEU20182006400 DEU20182006500 DEU20182007100 DEU20182007200 DEU20182008300 DEU20181092300 DEU20181092400 DEU20181092800 DEU20181092900 DEU20181093800 DEU20181093900 DEU20181094500 DEU20181094600 DEU20181095500 DEU20181095600 DEU20181095700 DEU20181095800 DEU20181095900 DEU20181097300 DEU20181097400 DEU20182242900 DEU20181106000 DEU20181106100 DEU20181130500 DEU20181130600 DEU20181130700 DEU20181131200 DEU20181131400 DEU20181132300 DEU20181136600 DEU20181137000 DEU20181137100 DEU20181137600 DEU20181148900 DEU20181149000 DEU20181149100 DEU20181159300 DEU20181161500 DEU20182583800 DEU20182583900 DEU20181206000 DEU20181217100 DEU20181237000 DEU20181237600 DEU20181237700 DEU20181238900 DEU20181239500 DEU20181245600 DEU20181245700 DEU20181248500 DEU20181248600 DEU20181268500 DEU20181275400 DEU20181277600 DEU20181277800 DEU20181294900 DEU20181295000 DEU20181295100 DEU20181317900 DEU20181318000 DEU20181318600 DEU20181319900 DEU20181320600 DEU20181322100 DEU20181322200 DEU20181322600 DEU20181331200 DEU20181331300 DEU20181332400 DEU20181336500 DEU20181369200 DEU20181369800 DEU20181371000 DEU20181371300 DEU20181371400 DEU20181374300 DEU20181378100 DEU20181378900 DEU20181379000 DEU20181379200 DEU20181379800 DEU20181399900 DEU20181411000 DEU20181414100 DEU20181418900 DEU20181419000 DEU20181422500 DEU20181422600 DEU20181422700 DEU20181423500 DEU20181423600 DEU20181424300 DEU20181424500 DEU20181425200 DEU20181426800 DEU20181430400 DEU20181435500 DEU20181439600 DEU20181439700 DEU20181440500 DEU20181447600 DEU20181448300 DEU20181448500 DEU20181448600 DEU20181454200 DEU20181454300 DEU20181481800 DEU20181481900 DEU20181482000 DEU20181482600 DEU20181482700 DEU20181484300 DEU20181484400 DEU20181484500 DEU20181486100 DEU20181497500 DEU20181497600 DEU20181497700 DEU20181498300 DEU20181498400 DEU20181499200 DEU20181499300 DEU20181513300 DEU20181513400 DEU20181525900 DEU20181526000 DEU20181529500 DEU20181529900 DEU20181530000 DEU20181530700 DEU20181531400 DEU20181531500 DEU20181532200 DEU20181532300 DEU20181542100 DEU20181542200 DEU20181542300 DEU20181543000 DEU20181543100 DEU20181543200 DEU20181543300 DEU20181543400 DEU20181576700 DEU20181576800 DEU20181590700 DEU20181591000 DEU20181638700 DEU20181653000 DEU20181668800 DEU20181670500 DEU20181671100 DEU20181679600 DEU20181679700 DEU20181681300 DEU20181681600 DEU20181681700 DEU20181690900 DEU20181691000 DEU20181691100 DEU20181712000 DEU20181712100 DEU20181712200 DEU20181712300 DEU20181714100 DEU20181714200 DEU20181714300 DEU20181714900 DEU20181722200 DEU20181728300 DEU20181728400 DEU20181762600 DEU20181763400 DEU20181763500 DEU20181765700 DEU20181765800 DEU20181769800 DEU20181772700 DEU20181772800 DEU20181781000 DEU20181781100 DEU20181790500 DEU20181793500 DEU20181793900 DEU20181794000 DEU20181794100 DEU20181806800 DEU20181813800 DEU20181814500 DEU20181816100 DEU20181825900 DEU20181826000 DEU20181826400 DEU20181836300 DEU20181836400 DEU20181837000 DEU20181853000 DEU20181857100 DEU20181857200 DEU20181857800 DEU20181858100 DEU20181868900 DEU20181875600 DEU20181883800 DEU20181886600 DEU20181887500 DEU20181985100 DEU20181985700 DEU20181985900 DEU20181986000 DEU20181989100 DEU20181989200 DEU20181989300 DEU20181989800 DEU20181989900 DEU20181991800 DEU20181993200 DEU20181995800 DEU20181995900 DEU20181996000 DEU20181996100 DEU20181996800 DEU20181996900 DEU20181998200 DEU20181999800 DEU20181999900 DEU20182000500 DEU20182000600 DEU20182001100 DEU20182002800 DEU20182003300 DEU20182005200 DEU20182007500 DEU20182008400 DEU20182008500 DEU20182008600 DEU20182010700 DEU20182011900 DEU20182013800 DEU20182014000 DEU20182014100 DEU20182072800 DEU20182074500 DEU20182074600 DEU20182090500 DEU20182090700 DEU20182091900 DEU20182097000 DEU20182097100 DEU20182097800 DEU20182127900 DEU20182128000 DEU20182128100 DEU20182128800 DEU20182128900 DEU20182129100 DEU20182129200 DEU20182129900 DEU20182170100 DEU20182170300 DEU20182170400 DEU20182170800 DEU20182170900 DEU20182171000 DEU20182171300 DEU20182171400 DEU20182171500 DEU20182171600 DEU20182172500 DEU20182172600 DEU20182172700 DEU20182172800 DEU20182172900 DEU20182173000 DEU20182173100 DEU20182173200 DEU20182173300 DEU20182173400 DEU20182173500 DEU20182173600 DEU20182179900 DEU20182180000 DEU20182197100 DEU20182198100 DEU20182198200 DEU20182198300 DEU20182199000 DEU20182199400 DEU20182200000 DEU20182202500 DEU20182202700 DEU20182202800 DEU20182203500 DEU20182204000 DEU20182209000 DEU20182209800 DEU20182210400 DEU20182210500 DEU20182210600 DEU20182212300 DEU20182212700 DEU20182212800 DEU20182214000 DEU20182215100 DEU20182215200 DEU20182215300 DEU20182215400 DEU20182215500 DEU20182215600 DEU20182215700 DEU20182215800 DEU20182215900 DEU20182216000 DEU20182216100 DEU20182216300 DEU20182220500 DEU20182221400 DEU20182222500 DEU20182222600 DEU20182227000 DEU20182227100 DEU20182227200 DEU20182228500 DEU20182229300 DEU20182229400 DEU20182235800 DEU20182296800 DEU20182296900 DEU20182297400 DEU20182326500 DEU20182424700 DEU20182424800 DEU20182549300 DEU20182550000 DEU20182550700 DEU20182551200 DEU20182551500 DEU20182552500 DEU20182552600 DEU20182552700 DEU20182596800 DEU20182597500 DEU20182599300 DEU20182599800 DEU20182599900 DEU20182608400 DEU20182634000 DEU20182634200 DEU20182634600 DEU20182634700 DEU20182675700 DEU20182709200 DEU20182709300 DEU20182709900 DEU20182710200 DEU20182710300 DEU20182710800 DEU20181216600 DEU20181216700 DEU20171041300 DEU20171041400 DEU20171053300 DEU20171053400 DEU20171054000 DEU20171054100 DEU20171054500 DEU20171054600 DEU20171073600 DEU20171074200 DEU20171074300 DEU20171074400 DEU20171104700 DEU20171104800 DEU20171105400 DEU20171105900 DEU20171106000 DEU20171106100 DEU20171107000 DEU20171107800 DEU20171121300 DEU20171121400 DEU20171123200 DEU20171123300 DEU20171123400 DEU20171124200 DEU20171134300 DEU20171134800 DEU20171163600 DEU20171164100 DEU20171164200 DEU20171177600 DEU20171202200 DEU20171202500 DEU20171203500 DEU20171203600 DEU20171204100 DEU20171204200 DEU20171204700 DEU20171204800 DEU20171205500 DEU20171212500 DEU20171249600 DEU20171260000 DEU20171264300 DEU20171264400 DEU20171280000 DEU20171283200 DEU20171292300 DEU20171292400 DEU20171293100 DEU20171295500 DEU20171324400 DEU20171325000 DEU20171325200 DEU20171337100 DEU20171337200 DEU20171337300 DEU20171344200 DEU20171345200 DEU20171345900 DEU20171346000 DEU20171356500 DEU20171357100 DEU20171366000 DEU20171366700 DEU20171367300 DEU20171374600 DEU20171374700 DEU20171375500 DEU20171377000 DEU20171378000 DEU20171378300 DEU20171391300 DEU20171391400 DEU20171392000 DEU20171392300 DEU20171392400 DEU20171396100 DEU20171416000 DEU20171429000 DEU20171431700 DEU20171431800 DEU20171432600 DEU20171433400 DEU20171433500 DEU20171440800 DEU20171441700 DEU20171441800 DEU20171442500 DEU20171443800 DEU20171443900 DEU20171444500 DEU20171444600 DEU20171468900 DEU20171484800 DEU20171489100 DEU20171489200 DEU20171498700 DEU20171498800 DEU20171499400 DEU20171500300 DEU20171500400 DEU20171509100 DEU20171509200 DEU20171518200 DEU20171518300 DEU20171519000 DEU20171519100 DEU20171519900 DEU20171529500 DEU20171530300 DEU20171530400 DEU20171530500 DEU20171530600 DEU20171538100 DEU20171572700 DEU20171572800 DEU20171573700 DEU20171573800 DEU20171573900 DEU20171574800 DEU20171575500 DEU20171575600 DEU20171576500 DEU20171578300 DEU20171578400 DEU20171581800 DEU20171581900 DEU20171585600 DEU20171587200 DEU20171587300 DEU20171590900 DEU20171591500 DEU20171592600 DEU20171592700 DEU20171620000 DEU20171620100 DEU20171620700 DEU20171620800 DEU20171623600 DEU20171623700 DEU20171624400 DEU20171624500 DEU20171624600 DEU20171624700 DEU20171626100 DEU20171626200 DEU20171626300 DEU20171627000 DEU20171639100 DEU20171639200 DEU20171639300 DEU20171639400 DEU20171640300 DEU20171641100 DEU20171641200 DEU20171675300 DEU20171675400 DEU20171692900 DEU20171713800 DEU20171722400 DEU20171743600 DEU20171767900 DEU20171768200 DEU20171768300 DEU20171776900 DEU20171777400 DEU20171777500 DEU20171786200 DEU20171786400 DEU20171786500 DEU20171788000 DEU20171788800 DEU20171789100 DEU20171790000 DEU20171790300 DEU20171796100 DEU20171796200 DEU20171796600 DEU20171796700 DEU20171796800 DEU20171799400 DEU20171817400 DEU20171818500 DEU20171818600 DEU20171819400 DEU20171819800 DEU20171820800 DEU20171820900 DEU20171833400 DEU20171833500 DEU20171833600 DEU20171842300 DEU20171868600 DEU20171875800 DEU20171879300 DEU20171881900 DEU20171885200 DEU20171886200 DEU20171887000 DEU20171888100 DEU20171888200 DEU20171888300 DEU20171888400 DEU20171902500 DEU20171902600 DEU20171911600 DEU20171911700 DEU20171916200 DEU20171929400 DEU20171930100 DEU20171939400 DEU20171939500 DEU20171940400 DEU20171940500 DEU20171940600 DEU20171941300 DEU20171941600 DEU20171942300 DEU20171942400 DEU20171953900 DEU20171955200 DEU20171955300 DEU20171962100 DEU20171962200 DEU20171962700 DEU20171962800 DEU20171962900 DEU20171967000 DEU20171972100 DEU20171976700 DEU20171976800 DEU20171976900 DEU20171977900 DEU20171984000 DEU20171985800 DEU20171993900 DEU20172000600 DEU20172010500 DEU20172011300 DEU20172013700 DEU20172013800 DEU20172013900 DEU20172021000 DEU20172121000 DEU20172121100 DEU20172121300 DEU20172121700 DEU20172121800 DEU20172122000 DEU20172122900 DEU20172123800 DEU20172123900 DEU20172124000 DEU20172124100 DEU20172128500 DEU20172128600 DEU20172128700 DEU20172128800 DEU20172129000 DEU20172129200 DEU20172129500 DEU20172131700 DEU20172131800 DEU20172132800 DEU20172132900 DEU20172133000 DEU20172133100 DEU20172133400 DEU20172133500 DEU20172133600 DEU20172135700 DEU20172135800 DEU20172135900 DEU20172136800 DEU20172138100 DEU20172138200 DEU20172139200 DEU20172139500 DEU20172139600 DEU20172140100 DEU20172140200 DEU20172143100 DEU20172143700 DEU20172145000 DEU20172145200 DEU20172145400 DEU20172146300 DEU20172147700 DEU20172147900 DEU20172148000 DEU20172149000 DEU20172149100 DEU20172149200 DEU20172149500 DEU20172149800 DEU20172151200 DEU20172152300 DEU20172152400 DEU20172153700 DEU20172153800 DEU20172153900 DEU20172154900 DEU20172155000 DEU20172155100 DEU20172217400 DEU20172217500 DEU20172217600 DEU20172217700 DEU20172234500 DEU20172241400 DEU20172277900 DEU20172278000 DEU20172278100 DEU20172278800 DEU20172278900 DEU20172279500 DEU20172279900 DEU20172280000 DEU20172314400 DEU20172314500 DEU20172314600 DEU20172314700 DEU20172314800 DEU20172315300 DEU20172315700 DEU20172316500 DEU20172316700 DEU20172316800 DEU20172316900 DEU20172317000 DEU20172317100 DEU20172317200 DEU20172317300 DEU20172317400 DEU20172317500 DEU20172317600 DEU20172317700 DEU20172317800 DEU20172322500 DEU20172322600 DEU20172322700 DEU20172323000 DEU20172323100 DEU20172323200 DEU20172341100 DEU20172341400 DEU20172342800 DEU20172343100 DEU20172343400 DEU20172343500 DEU20172344000 DEU20172344100 DEU20172346700 DEU20172346900 DEU20172347000 DEU20172347600 DEU20172347700 DEU20172347800 DEU20172350100 DEU20172350200 DEU20172353100 DEU20172354600 DEU20172355200 DEU20172356900 DEU20172358600 DEU20172358800 DEU20172358900 DEU20172359500 DEU20172359600 DEU20172359700 DEU20172360100 DEU20172360200 DEU20172360300 DEU20172360400 DEU20172360500 DEU20172367100 DEU20172367300 DEU20172368000 DEU20172371800 DEU20172371900 DEU20172372300 DEU20172374800 DEU20172383800 DEU20172384100 DEU20172384200 DEU20172384900 DEU20172385000 DEU20172385200 DEU20172385300 DEU20172391500 DEU20172406300 DEU20172460000 DEU20172460600 DEU20172460700 DEU20172607300 DEU20172607400 DEU20172607600 DEU20172622500 DEU20172622800 DEU20172622900 DEU20172623000 DEU20172623600 DEU20172637900 DEU20172669300 DEU20172669400 DEU20172716500 DEU20172717000 DEU20172739400 DEU20172740600 DEU20172740700 DEU20172742700 DEU20172742900 DEU20172743000 DEU20172743400 DEU20172755700 DEU20172755800 DEU20172776700 DEU20172790500 DEU20172792700 DEU20172794400 DEU20172794500 DEU20172796500 DEU20172798400 DEU20172798500 DEU20172811300 DEU20172811400 DEU20172812700 DEU20172824300 DEU20172824400 DEU20172828700 DEU20172828800 DEU20172828900 DEU20172829500 DEU20172830300 DEU20172830400 DEU20172831100 DEU20172874200 DEU20172913800 DEU20172914400 DEU20172914500 DEU20172914600 DEU20172914800 DEU20181618700 

	checkData(x = as.data.frame(dt_deu))

# ==================
# EST	
# ==================
		
	source("func_checkData.r")		
	checkData(x = as.data.frame(dt_est), ignore_stop=TRUE)

#check: has vslLenCls "" or NA
	sum(is.na(dt_est$vslLenCls) | dt_est$vslLenCls=="") # n= 177
#check: has depDate "" or NA
	sum(is.na(dt_est$depDate) | dt_est$depDate=="") # n= 3910
#check: has depDate "" or NA
	sum(is.na(dt_est$landDate) | dt_est$depDate=="") # n= 3910

# check depLoc unknown		 
	FINKNA
# check landLoc unknown
	EEHDI EELYK EENAY EENEE EEKKR EEKMM EEPTK EEPRZ EETJS EEMVS EEPUK EELIY EEPUL
# landCat unknown		 
	"" # 815
	
checkData(x = as.data.frame(dt_est))

	
# ==================
# FIN	
# ==================

	source("func_checkData.r")	
	checkData(x = as.data.frame(dt_fin), ignore_stop=TRUE)
		
# minor: rename columns
	dt_fin$fishTripId <- dt_fin$fishTripid; dt_fin$fishTripid<-NULL
	
# minor: issues with formating	
	dt_fin$landWt<-as.numeric(dt_fin$landWt)
	dt_fin$depLoc<-as.character(dt_fin$depLoc)
	
# check: issues with metiers [non existing in CL]
	dt_fin$foCatEu6[dt_fin$foCatEu6=="OTM_DEF_>=105_1_12"]<-"OTM_DEF_>=105_1_120"
	
#check: all depLoc "" or NA
	sum(is.na(dt_fin$depLoc) | dt_fin$vslLenCls=="") # n= 10689
#check landing before departure, trips:
	dt_fin[fishTripId=="FIN20170003176",]
	
#check: Days at sea greater than 20, for  4 trips:
	# FIN20180001365 FIN20180001700 FIN20180003475 FIN20170003468 


		checkData(x = as.data.frame(dt_fin))
	
# ==================
# LTU	
# ==================

	source("func_checkData.r")
	checkData(x = as.data.frame(dt_ltu), ignore_stop=TRUE)

# minor: rename columns
	dt_ltu$vslLenCls <- dt_ltu$lenghtsegmnt; dt_ltu$lenghtsegmnt<-NULL
	dt_ltu$landCat <- dt_ltu$LangCAT01; dt_ltu$LangCAT01<-NULL

# minor: issues with formating			
	dt_ltu$sppCode <- as.integer(dt_ltu$sppCode)
	dt_ltu$landWt<-as.numeric(dt_ltu$landWt)

# check: issues with metiers [non existing in CL]	
	dt_ltu$foCatEu6[dt_ltu$foCatEu6=="GNS_SPF_16-31_0_0"]<-"GNS_SPF_16-109_0_0"	

# check ERROR more than one departure date, trips:
	# LTU20170000301 LTU20170001158 LTU20170001162 LTU20170000731 LTU20170000259 LTU20170000108
		
# check:  "27.3.d.28" area not in the code list
		sum(dt_ltu$area=="27.3.d.28") # n = 449

	
	checkData(x = as.data.frame(dt_ltu))
			
	
# ==================
# POL	
# ==================	

	source("func_checkData.r")
	checkData(x = as.data.frame(dt_pol), ignore_stop=TRUE)

# minor: issues with formating	
	dt_pol$sppName<-as.character(dt_pol$sppName)
	dt_pol$landWt<-as.numeric(gsub(",",".", dt_pol$landWt))
		
#check has depDate "" or NA
	sum(dt_pol$depDate=="NA") # n= 7679

# check depLoc unknown		
	LVLPA LVVEN  DKNXO PLFRO PLUSM PLGDA 
	
	
# check landLoc unknown		
	LVLPA LVVEN  DKNXO PLFRO PLUSM PLGDA  

# check:  "27.3.d.28" area not in the code list
	sum(dt_pol$area=="27.3.d.28") # n = 347

	
		checkData(x = as.data.frame(dt_pol))

# ==================
# LVA	
# ==================		
	
	source("func_checkData.r")
	checkData(x = as.data.frame(dt_lva), ignore_stop=TRUE)

# minor: issues with formating		
	dt_lva$sppCode <- as.integer(dt_lva$sppCode)
	dt_lva$landWt<-as.numeric(dt_lva$landWt)		
# check fix: landDate wrong format	
	ls1<-strsplit(dt_lva$landDate,"-")
	ls2<-lapply(ls1, function(x){if(nchar(x[3])==1) {x[3]<-paste(0,x[3],sep="")}; if(nchar(x[2])==1) {x[2]<-paste(0,x[2],sep="")}; x})
	df1<-do.call("rbind", ls2)	
	dt_lva$landDate<-apply(df1, 1, paste, collapse="-")
	dt_lva$landWt<-as.numeric(dt_lva$landWt)
# check fix: replace: pair (714851,"Clupea harenguss" ) by pair (126417, "Clupea harengus")
	dt_lva$sppCode[dt_lva$sppCode==714851]<-126417
	dt_lva$sppName[dt_lva$sppName=="Clupea harenguss"]<-"Clupea harengus"
# check fix: replace: sppName "Osmerus epelanus" by sppName "Osmerus eperlanus")
	dt_lva$sppName[dt_lva$sppName=="Osmerus epelanus"]<-"Osmerus eperlanus"
# check fix: replace: sppCode 405822 [Spratella sprattus baltica] by sppCode 126425 [Sprattus sprattus]
	dt_lva$sppCode[dt_lva$sppCode==405822]<-126425
# check: all depDate "" or NA
	table(dt_lva$depDate, useNA="al")
# check: all depLoc "" or NA
	table(dt_lva$depLoc, useNA="al")
# check: all rect "" or NA
	table(dt_lva$rect, useNA="al")
	
# check: landCat unknown		 
	BMS # 1
	GUT # 1
	WHL	# 12554
	
# ==================
# DNK	
# ==================		
	
	source("func_checkData.r")
	checkData(x = as.data.frame(dt_dnk), ignore_stop=TRUE)
	
# WARNING more than one landing date, trips:
	# DNK20170005731 DNK20180080369 DNK20180080543 DNK20180081467 DNK20170146233 DNK20170326720 DNK20170331075 DNK20170331080 DNK20180334066 DNK20170338627 DNK20180497547 DNK20170588180 DNK20170589965 DNK20170598462 DNK20170728240 DNK20170795606 DNK20170795977 DNK20170796190 DNK20170796678

# check fix: issues with metiers [non existing in CL]
	dt_dnk$foCatEu6[dt_dnk$foCatEu6=="FPN_MOL_>0_0_0"]<-"MIS_MIS_0_0_0"

# check depLoc unknown		
	# DKQSB	
# check landLoc unknown		
	# DKQSB	
	
		checkData(x = as.data.frame(dt_dnk))

# ==================
# SWE	
# ==================

	source("func_checkData.r")
	# SWE ok
		checkData(x = as.data.frame(dt_swe))



