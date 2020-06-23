

# checking function for fishPi2 data submission 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# checkData(x)
# this function runs checks on the expected data frame x 
# and print to the screen any errors in the data. 
# only isn the case of depDate and landDate will the function stop
# requires the "fishPiCodes" package
# AP Feb 2018

# adaptations by NP
# v2 13-3-2018 area codes 27.3.a and 27.3.a.20 added
# v3 29-04-2019 adapted to project "rcg_ba_small_pelagics": 3 new rectangles, 4 new sppCodes and sppNames
# v3 09-05-2019 extended adaptations to project "rcg_ba_small_pelagics": 3 new rectangles, 1 new sppCodes and sppNames
# v3 09-05-2019 added argument ignore_stop: allows function to run and check all errors without stopping
# 06-06-2019 - Kirsten added missing NA to dates string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

checkData <-function(x, ignore_stop=FALSE)
{

b <-x
# setting up the code lists

library(fishPiCodes)

data(metiers)

metiers <-as.character(metiers$level6)
metiers <- unique(c(metiers,"FPN_CRU_>0_0_0","OTM_SPF_16-104_0_0"))


data(rectangles)
rects <-unique(c(as.character(rectangles$statsq),"54G7","48G8","47G7","55H2","50H7","36G4","37F9","37G0","36G3"))

data(ASFIS_WoRMS)
sppCodes <- c(ASFIS_WoRMS$AphiaID,254529, 125476,125517,405451,11676, 151324, 236462)
#sppCodes <- c(ASFIS_WoRMS$AphiaID_accepted,254529, 125476,125517,405451,11676, 151324, 236462)
sppNames <- unique(c(as.character(ASFIS_WoRMS$ScientificName), "Myoxocephalus quadricornis","Gasterosteidae","Anarhichadidae","Gymnocephalus cernua","Pisces","Gasterosteus aculeatus aculeatus"))

data(UNLOCODE)
locs <-unique(c(as.character(UNLOCODE$loCode), "SEEKE", "DESBS"))

areas <-c("27.3.a.20","27.3.a.21","27.3.b.23","27.3.c.22","27.3.d.24","27.3.d.25","27.3.d.26", "27.3.d.27","27.3.d.28.1","27.3.d.28.2","27.3.d.29","27.3.d.30","27.3.d.31","27.3.d.32")

vslLenCls <-c("VL0008","VL0810","VL1012","VL1218","VL1824","VL2440","VL40XX")

vslFlgCtry <-c("DEU", "DNK", "SWE", "POL","FIN","LVA","EST","LTU")

dates <- c(as.character(seq(as.Date("2016-12-01"), as.Date("2019-01-31"), "days")), c(NA))

# summary functions
lengthUnique <-function(x){length(unique(x))}

# --------------------------------------------
# CHECKS
# standard checks of the fishPi2 data format
# --------------------------------------------
# class
if(class(b)!="data.frame")
{
stop("x is not a data frame")
}
# checks columns 
# all column names present and in the correct order
colNames <-c("vslFlgCtry","vslId","vslLenCls","fishTripId","depDate","depLoc","landDate","landLoc","rect","area","foCatEu6","sppCode","sppName","landWt","landCat")

if(all(colNames %in% names(b))){cat("OK all expected column names present\n")}
if(!all(colNames %in% names(b))){cat("ERR not all expected column names present\n"); cat("columns missing:\n", paste(colNames[!colNames %in% names(b)], collapse="\n ")); cat("\n");stop()}

if(all(names(b) %in% colNames)){cat("OK only the expected column names present\n")}
if(!all(names(b) %in% colNames)){cat("ERR more than expected column names present\n"); cat("columns too much:\n", paste(names(b)[!names(b) %in% colNames], collapse="\n ")); cat("\n")
print(paste(".removing extra column\n",names(b)[!names(b) %in% colNames]))
b<-b[,names(b) %in% colNames]
}

if(all(order(names(b))==order(colNames))){cat("OK all df colums in order\n")}
if(!(all(order(names(b))==order(colNames)))){cat("ERROR df colums in wrong order\n")
print(paste(".reordering columns"))
b<-b[,colNames]
}

# checks for character variable

charVar <-c("vslFlgCtry","vslId","vslLenCls","fishTripId","depDate","depLoc","landDate","landLoc","rect","area","foCatEu6","sppName")
for(i in 1:length(charVar))
{
eval(parse(text=paste("var <-b$",charVar[i],sep="")))
{
if(class(var) !="character"){cat("ERROR",charVar[i], "variable not character\n")}
if(class(var) =="character"){cat("OK",charVar[i],"variable is character\n")}
}
}

if(class(b$sppCode) !="integer"){cat("ERROR sppCode variable not an integer\n")}
if(class(b$landWt) =="integer"){cat("OK sppCode variable is an integer\n")}
if(class(b$landWt) !="numeric"){cat("ERROR landWt variable not numeric\n")}
if(class(b$landWt) =="numeric"){cat("OK landWt variable is numeric\n")}

# checks that vslFlgCtry is in the code list

if(all(b$vslFlgCtry %in% vslFlgCtry)){cat("OK vslFlgCtry in code list\n")}
if(!(all(b$vslFlgCtry %in% vslFlgCtry))){cat("ERROR vslFlgCtry not code list\n")}

# vslId 
# length of character string 

if(all(nchar(b$vslId)==8)){cat("OK vslId has correct number of characters\n")}
if(all(nchar(b$vslId)!=8)){cat("ERROR vslId has incorrect number of characters\n")}


# leading flgCtry
if(all(substr(b$vslId,1,3) %in% vslFlgCtry)){cat("OK vslId has flgCtry component\n")}
if(!(all(substr(b$vslId,1,3) %in% vslFlgCtry))){cat("ERROR vslId does not have correct flgCtry component\n")}

# vlsLenCls in code list
if(all(b$vslLenCls %in% vslLenCls)){cat("OK vslLenCls in code list\n")}
if(!(all(b$vslLenCls %in% vslLenCls))){cat("ERROR vslLenCls not code list\n")
print ("situations:")
print(table(b$vslLenCls[!b$vslLenCls %in% vslLenCls], useNA="al"))
}


# fishTripId 
if(all(nchar(b$fishTripId)==14)){cat("OK fishTripId has correct number of characters\n")}
if(all(nchar(b$fishTripId)!=14)){cat("ERROR fishTripId has incorrect number of characters\n")}

if(all(substr(b$fishTripId,1,3) %in% vslFlgCtry)){cat("OK fishTripId has flgCtry component\n")}
if(!(all(substr(b$fishTripId,1,3) %in% vslFlgCtry))){cat("ERROR fishTripId does not have correct flgCtry component\n")}


# only one vslId per fishTripId 
maxVslTrip <-max(tapply(b$vslId,b$fishTripId,lengthUnique))
if(maxVslTrip >1){cat("ERROR more than one vessel for trip\n")}
if(maxVslTrip ==1){cat("OK all trips have a single vessel ID \n")}


#browser()
# dates format 
if(!(all(b$depDate %in% dates)))
{
cat("ERROR departure date not in format or range\n")
cat(unique(b$depDate[which((b$depDate %in% dates)==FALSE)]),"\n")
if(!ignore_stop==TRUE) stop("Cant progress until this is fixed\n")
}
if(!(all(b$landDate %in% dates)))
{
cat("ERROR landing date not in format or range\n")
cat(unique(b$landDate[which((b$landDate %in% dates)==FALSE)]),"\n")
if(!ignore_stop==TRUE) stop("Cant progress until this is fixed\n")
}

if(all(b$depDate %in% dates)){cat("OK all depDate in format and range\n")}
if(all(b$landDate %in% dates)){cat("OK all landDate in format and range\n")}

# landDate after depDate
# this is the daysAtSea variable
depDates <-as.Date(b$depDate)
landDates <-as.Date(b$landDate)
daysAtSea <-difftime(landDates,depDates,units="days")


if(all(daysAtSea >=0, na.rm=T)){cat("OK landing dates same as or after departure dates\n")} 
if(any(daysAtSea <0, na.rm=T))
{cat("ERROR landing before departure, trips:\n")
cat(unique(b$fishTripId[which(daysAtSea<0)]),"\n")
}

if(any(daysAtSea >20, na.rm=T))
{
longTrips <-unique(b$fishTripId[which(daysAtSea>20)])
cat("Days at sea greater than 20, for ",length(longTrips),"trips:\n")
cat(longTrips,"\n")
}

depTab <-tapply(b$depDate,b$fishTripId,lengthUnique)
if(max(depTab>1))
{cat("ERROR more than one departure date, trips:\n")
cat(unique(b$fishTripId[which(depTab>1)]),"\n")
}

landTab <-tapply(b$landDate,b$fishTripId,lengthUnique)
if(max(landTab>1))
{cat("WARNING more than one landing date, trips:\n")
cat(unique(b$fishTripId[which(landTab>1)]),"\n")
}


# depLoc and landLoc
# in code list 

if(all(b$depLoc %in% locs)){cat("OK depLocs in code list \n")}
if(!(all(b$depLoc %in% locs)))
{
cat("ERROR depLoc not in the code list\n")
misdepLoc <-unique(b$depLoc[which(!(b$depLoc %in% locs))])
cat(misdepLoc,"\n") 
}

if(all(b$landLoc %in% locs)){cat("OK landLocs in code list \n")}
if(!(all(b$landLoc %in% locs)))
{
cat("ERROR landLoc not in the code list\n")
mislandLoc <-unique(b$landLoc[which(!(b$landLoc %in% locs))])
cat(mislandLoc,"\n") 
}
# extent of LOCODEs that are 999 
uniLocs <-unique(c(b$depLoc,b$landLoc))
locsUnknown <-uniLocs[grep("999",uniLocs)]
missingLocsPC <-round(length(locsUnknown)/length(uniLocs),5)
cat(100*missingLocsPC,"% of locations are 999\n")
tripsUnknown <-unique(b$fishTripId[b$landLoc %in% locsUnknown|b$depLoc %in% locsUnknown])
uniFishTrips <-unique(b$fishTripId)
missingTripsPC <-round(length(tripsUnknown)/length(uniFishTrips),5)
cat("and",100*missingTripsPC,"% of trip locations are 999\n")


# rectangles 
if(all(b$rect %in% rects)){cat("OK rectangles in code list \n")}
if(!(all(b$rect %in% rects)))
{
cat("ERROR rectangles not in the code list\n")
misRects <-unique(b$rect[which(!(b$rect %in% rects))])
cat(misRects,"\n") 
}


# areas
if(all(b$area %in% areas)){cat("OK area in code list \n")}
if(!(all(b$area %in% areas)))
{
cat("ERROR area not in the code list\n")
misAreas <-unique(b$area[which(!(b$area %in% areas))])
cat(misAreas,"\n") 
}


# metier
if(all(b$foCatEu6 %in% metiers)){cat("OK foCatEu6 in metier list \n")}
if(!(all(b$foCatEu6 %in% metiers)))
{
cat("ERROR foCAtEu6 not in the metier list\n")
misMets <-unique(b$foCatEu6[which(!(b$foCatEu6 %in% metiers))])
cat(misMets,"\n") 
}

# species Ahpia IDs
if(all(b$sppCode %in% sppCodes)){cat("OK sppCode in species list \n")}
if(!(all(b$sppCode %in% sppCodes)))
{
cat("ERROR sppCode not in the species list \n")
misSpp <-unique(b$sppCode[which(!(b$sppCode %in% sppCodes))])
cat(misSpp,"\n") 
}


# species Names IDs
if(all(b$sppName %in% sppNames)){cat("OK sppName in species list \n")}
if(!(all(b$sppName %in% sppNames)))
{
cat("ERROR sppName not in the species names list \n")
misSppName <-unique(b$sppName[which(!(b$sppName %in% sppNames))])
cat(misSppName,"\n") 
}

return("All done")
}
