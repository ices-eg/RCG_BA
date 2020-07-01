
importNationalLandings <- function(fname, year, VN, ports, sheet = "FAR"){
  traal <- loadExcelSheetData(fname, year, ports, sheet)
  #bringing data to common items
  traal$speciesCode <- sub("*", "",traal$speciesCode, fixed = TRUE)
  inter <- reshape2::colsplit(traal$zoneLong, " ", c("zone","place"))
  inter$zone <- sub("-", ".",inter$zone, fixed = TRUE)
  traal <- cbind(traal, inter)
  traal$vessel <- tolower(traal$vessel)
  # date
  traal$date <- lubridate::date(traal$catchDate)
  
  #add vessel ids to all trips
  traal <- merge(traal, VN[,c("VNname", "VDid")],
        by.x = "vessel", by.y = "VNname", all.x = TRUE)
  
  if(any(is.na(traal$VDid))){
    missing <- missingVals(traal, "VDid", "vessel")
    missing<- paste0(unique(missing), collapse = ", ")
    warning(paste(missing, "are missing in the Vessel Details table!"))
  }
  
  
  return (traal)
  
}

loadExcelSheetData <- function(fname, year, ports, sheet){
  if(sheet=="FAR") return(loadFAR(fname, year))
  if(sheet=="LAN") return(loadLAN(fname, year))
  if(sheet=="DEP") {
    df <- loadDEP(fname, year)
    df <- addPortCode(df, ports, "portDEP", "portDEPid")
    return(df)
  }
  if(sheet=="RTP") {
    df<-loadRTP(fname, year)
    df <- addPortCode(df, ports, "portRTP", "portRTPid")
    return(df)
  }
  stop(paste(sheet,"importing not implemented!"))
}


loadFAR <- function(fname, year){
  if(year %in% c(2019)){
    toSkip <- 8
    colTypes <- c("skip", "text", "numeric", "text", "text", "date", 
                  "date", "numeric", "date", "text", "text",
                  "text", "text", "text", "text", "numeric", 
                  "text", "date", "text","text", 
                  "date", "text", "numeric", "date", "text",
                  "text", "numeric", "numeric", "numeric", "numeric",
                  "text", "text", "text", "skip", "skip")
    newNames <- c("icesPR", "totTimeH", "tripCode", "vessel", "messageDate", 
                 "catchDate", "hauls", "netInTime", "fao",  "vmsPR", 
                 "icesPR2", "zoneLong", "species", "speciesCode",  "catchKg",
                 "sizeClass", "dateIn", "netInLat", "netInLon", 
                 "netOutTime", "quarter", "month", "dateOut", "netOutLat", 
                 "netOutLon", "mesh", "netHeight", "netWidth", "catchDepth", 
                 "status", "gear", "nameGearET")
    
    
  }
  else{stop("This year not implemented!")}
  
  df <- readxl::read_excel(fname, sheet = "FAR",
                           col_types =  colTypes, 
                           skip = toSkip)
  #TODO check if all requred columns exist before renaming
  df <- reNameColumns(df, newNames)
  
  #convet to lubridate dates
  df$netInTime <- lubridate::ymd_hms(df$netInTime)
  df$netOutTime <- lubridate::ymd_hms(df$netOutTime)
  return(df)
}

loadLAN <- function(fname, year){
  if(year %in% c(2019)){
    toSkip <- 8
    colTypes <- c("text", "text", "text", "skip","text", "text",
                  "text", "numeric", "text", "date", "numeric", 
                  "numeric", "numeric", "numeric",  "text", 
                  "text", "text", "text", "skip", "text", 
                  "skip","skip","skip","skip","skip")
    
    newNames<- c("tripCode", "owner", "vessel", "zoneLong", "species",
                 "speciesCode", "month", "quarter", "catchDate",
                 "WeightKg", "WeightT","LiveWeightKg","LiveWeightT",
                 "preserved", "presented", "portCountry", "portName", "pk")
  }
  else{stop("This year not implemented!")}
  df <- readxl::read_excel(fname, sheet = "LAN",
                           col_types =  colTypes, 
                           skip = toSkip)
  #TODO check if all requred columns exist before renaming
  df <- reNameColumns(df, newNames)
  return(df)
  
}


reNameColumns <- function(df, newNames){
  # extra check to see if all seems correct
  if(ncol(df) != length(newNames)){
    stop(paste0("Number of new columns names is ", length(newNames),
                " the imported excel has ", ncol(df), " columns!"))
   }
   colnames(df) <- newNames
  
  return(df)
}


loadRTP <- function(fname, year){
  if(year %in% c(2019)){
    toSkip <- 9
    colTypes <- c("text", "text", "text",
                  "text", "text", "text", 
                  "numeric", "date", "date",
                  "date", "date", "text")
    newNames <- c("tripCode", "vessel", "internalID", 
                  "portRTP", "portID", "reasonRTP", 
                  "msgCount", "timeRTP", "dateRTP",
                  "msgTimeRTP", "msgDateRTP", "statusRTP")
    
    df <- readxl::read_excel(fname, sheet = "RTP",
                             col_types =  colTypes, 
                             skip = toSkip)
  }
  else{stop("This year not implemented!")}
  df <- reNameColumns(df, newNames)
  
  df$timeRTP <- lubridate::ymd_hms(df$timeRTP)
  
  return(df)
}

addPortCode <- function(df, ports, portNameCol, resultCol){
  startrows <- nrow(df)
  df <- merge(df, ports,
              by.x=portNameCol,  by.y = "harbor", all.x = T)
  if(nrow(df) != startrows) stop("Merge error, likely multiple ports in table!")
  df[,resultCol] <- df$port_code
  df$port_code <- NULL
  return(df)
}


loadDEP <- function(fname, year){
  if(year %in% c(2019)){
    toSkip <- 8
    colTypes = c("text", "text", "text",
                 "text", "text", "numeric", 
                 "text", "date", "date",
                 "text", "text", "date")
    
    newNames <- c("tripCode", "vessel", "internalID", 
                  "action", "speciesFAO", "fishWeight",
                  "gear", "timeDEP", "dateDEP", 
                  "countryDEP", "portDEP", "msgTimeDEP")
    df <- readxl::read_excel(fname, sheet = "DEP",
                             col_types =  colTypes, 
                             skip = toSkip)
  }
  
  else{stop("This year not implemented!")}
  
  df <- reNameColumns(df, newNames)
  
  df$timeDEP <- lubridate::ymd_hms(df$timeDEP)
  
  return(df)
}
