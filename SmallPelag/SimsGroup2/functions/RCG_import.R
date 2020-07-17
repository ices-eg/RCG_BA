RCGtable <- function(n){
  res <- list(
    `year` = character(n),
    `vslFlgCtry` = character(n),
    `vslId` = character(n),
    `vslLenCls` = character(n),
    `fishTripId` = character(n),
    `haulId` = character(n),
    `depDate` = character(n),
    `depLoc` = character(n),
    `arrDate` = character(n),
    `arrLoc` = character(n),
    `landDate` = character(n),
    `landLoc` = character(n),
    `rect` = character(n),
    `area` = character(n),
    `foCatEu6` = character(n),
    `sppCode` = integer(n),
    `sppName` = character(n),
    `stockCode` = character(n),
    `landWt` = numeric(n),
    `depQuarter` = integer(n),
    `depMonth` = integer(n),
    `depWeek` = integer(n),
    `arrQuarter` = integer(n),
    `arrMonth` = integer(n),
    `arrWeek` = integer(n),
    `landQuarter` = integer(n),
    `landMonth` = integer(n),
    `landWeek` = integer(n)
  )
  return(res)
}


far2rcg <- function(year, farRaw, dep, rtp, VD, country){
  
  if(country == "EE"){
    farRaw<-calcRCGTableValues(dep, rtp, farRaw, VD)
    df <- RCGtable(nrow(farRaw))
    
    df$year <- as.character(year)
    df$vslFlgCtry <- farRaw$VDflagCountry
    df$vslId <- farRaw$vessel #use the actual vessel name
    df$vslLenCls <- farRaw$VDlengthCategory
    df$fishTripId <- farRaw$tripCode
    df$haulId <- paste0(df$fishTripId, "_0", farRaw$haulNr)
    df$depDate <- as.character(farRaw$timeDEP, "%Y-%m-%d")
    df$depLoc <-farRaw$portDEPid
    df$arrDate <- as.character(farRaw$timeRTP, "%Y-%m-%d")
    df$arrLoc <- farRaw$portRTPid
    df$landDate <-df$arrDate #maybe take from landings table
    df$landLoc <- df$arrLoc #maybe take from landings table
    df$rect <- farRaw$icesPR
    df$area <- paste0("27.3.d.", farRaw$zone)
    df$foCatEu6 <- farRaw$met6
    df$sppCode <- spCharCodes2intCodes(farRaw$speciesCode) 
    df$sppName <- spIntCodes2SciNames(as.character(df$sppCode))
    df$stockCode <- "not specified"
    # for simplicity not adding landing Wt as it is not defined by haul
    # if needed can be aproximated by haul
    # use at sea catch estimation as landWt
    df$landWt <- farRaw$catchKg
    timeVals <- c("quarter","month", "week" )
    
    depTimeCols <- paste0("dep", up1(timeVals))
    df[depTimeCols] <- date2SeparateValues(farRaw$timeDEP, timeVals, "dep")
    
    arrTimeCols <- paste0("arr", up1(timeVals))
    df[arrTimeCols] <- date2SeparateValues(farRaw$timeRTP, timeVals, "arr")
    
    #setting landing to RTP maybe take landing values instead?!
    lanTimeCols <- paste0("land", up1(timeVals))
    df[lanTimeCols] <- date2SeparateValues(farRaw$timeRTP, timeVals, "land")
    
    res <- as.data.frame(df, stringsAsFactors = F)
    return(res)
  }
  
  else{stop(paste(country, "data import not implemented!"))}
  
}

getTrips <- function(dep, far, rtp){
  #join dep far and ret by tripcode
  trips <- merge(unique(dep[c("tripCode", "timeDEP", "portDEPid")]),
                 unique(
                   rtp[tolower(rtp$statusRTP) =="kehtiv",
                       c("tripCode", "timeRTP", "portRTPid")]),
                 by = "tripCode")
  if(max(table(trips$tripCode)) != 1) {
    msg <- "DEP or RTP have more than 1 entry! see trips:"
    tripCodes <- paste0(names(table(trips$tripCode)[table(trips$tripCode) > 1]),
                        collapse=", ")
    stop(paste(msg, tripCodes ))
  }
  #get all the fishing hauls by trip 
  fishHauls <- unique(far[tolower(far$status) =="kehtiv",
                          c("tripCode", "netInTime")])
  #order the hauls for correct haul number
  fishHauls <- fishHauls[order(fishHauls$netInTime),]
  
  fishHauls$haulNr <- ave(fishHauls$tripCode, fishHauls$tripCode,  FUN=seq_along)
  
  trips<-merge(trips, fishHauls, by="tripCode")
  
  return(trips)
}




calcRCGTableValues <- function(dep, rtp, far, VD){
  if(hascols(far, "haulNr")){stop("Haul number column exists in data!")}
  far$met6 <- calcMet6(far)
  far <- merge(far, VD, by="VDid")
  trips <- getTrips(dep,far, rtp)
  newFar <- merge(far, trips, by=c("tripCode", "netInTime"))
  # trip 33503 is a good example in year 2019 of multiple rectangles and days
  if(nrow(far) != nrow(newFar)){
    stop("Merge failed see code!")
  }
  
  return(newFar)
}

calcMet6 <- function(far){
  far$met6 <- ifelse(far$mesh > 15 & far$mesh <105 & far$gear == "OTM",
                     "OTM_SPF_16-104_0_0", NA)
  if(any(is.na(far$met6))) {
    missing <- unique(missingVals(far,"met6","gear"))
    missing <- paste0(missing, collapse = ", ")
    stop(paste("Undefined metier6:", missing))
  }
  return(far$met6)
}







