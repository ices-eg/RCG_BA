
createOverviewTbl <- function(dataObj) {
  # Check if data is a RBDESDataObject
  if (!inherits(dataObj, "RDBESDataObject")) {
    stop("Input must be a dataObject.")
  }
  DE <- dataObj$DE

  if(is.null(DE)){
    stop("No DE data found in the dataObject.")
  }

  #extact hierarchy in DE
  hierarchies <- unique(DE$DEhierarchy)
  if(length(hierarchies) > 1){
    stop("Only single hierarchy dataObjects are supported.")
  }
  supportedHierarchies <- c(1,2,3,4,8)
  if(!(hierarchies %in% supportedHierarchies)){
    stop(paste0("Hierarchy ", hierarchies,
                " is not supported. Supported hierarchies are: ",
                paste(supportedHierarchies, collapse = ", "), "."))
  }

  estObj <- createRDBESEstObject(dataObj)

  # Create overview table
  reqCommonFields <- c(
    BVfishId = "fishId",
    FMid = "FMid",
    SAid = "SAid",
    BVincProb = "BVincProb",
    BVvalueMeas = "BVvalueMeas",
    BVvalUnitScale = "BVvalUnitScale",
    DEyear = "year",
    SDctry = "country",
    DEsampScheme =  "samplingScheme",
    DEstratumName = "samplingFrame", #(DEstratum )
    DEhierarchy = "upperHierarchy",
    SAspeCode = "species",# (AphiaID)
    SAlowHierarchy = "lowerHierarchy"
  )

  reqH123Fields <- c(
    FOarea = "area",
    FOmetier6 = "metier",
    FOstatRect =  "rectangle",
    FOendDate = "date",
    FTid = "tripId"
  )

  reqH48Fields <- c(
    LEarea = "area",
    LEmetier6 = "metier",
    LEstatRect =  "rectangle",
    LEdate = "date",
    LEid = "tripId"
  )

  #keep only required columns from the est object based on hierarchy
  if(hierarchies %in% c(1,2,3)){
    reqFields <- c(reqCommonFields, reqH123Fields)
  } else if(hierarchies %in% c(4,8)){
    reqFields <- c(reqCommonFields, reqH48Fields)
  } else {
    stop(paste0("Hierarchy ", hierarchies,
                " is not supported for overview table creation."))
  }


  #subset estObj data.table to required columns
  estObj <- estObj[, names(reqFields), with = FALSE]

  #rename data.table columns
  data.table::setnames(estObj, old = names(reqFields), new = unname(reqFields))

  #take out rows with NA lowerHierarchy we have no sample there because somwwhere higer
  #there is likely No (N) in the sampled column
  estObj <- estObj[!is.na(lowerHierarchy)]
  
  #add quarter column from date
  estObj$quarter <- as.integer(format(as.Date(estObj$date), "%m")) %/% 4 + 1

  #ensure uniqueness of fishId
  estObj[, fishId := paste0(fishId, "_FM", FMid, "_SA", SAid)]

  # Add age, length, weight columns based on BV
  flatEstObj <- flattenEstObject(estObj)

  #add the inclusionProbability column as NA
  flatEstObj[, inclusionProbability := NA]

  finalColumns <- c(
    "year",
    "country",
    "samplingScheme",
    "samplingFrame", #(DEstratum )
    "upperHierarchy",
    "lowerHierarchy",
    "area",
    "rectangle",
    "quarter", #get from date
    "metier",
    "fishId",
    "tripId",
    "species",# (AphiaID)
    "age",
    "length",
    "weight",
    "inclusionProbability"
  )

  # Reorder data.table columns
  flatEstObj <- flatEstObj[, ..finalColumns]

  return(flatEstObj)



}


flattenEstObject <- function(estObj) {

  BVcols <- c("fishId", "BVincProb", "BVvalUnitScale",
              "BVvalueMeas")

  keepCols <- c(setdiff(names(estObj), BVcols), "fishId")
  
  
  flatBV <- flattenBVData(unique(estObj[, ..BVcols]))
  #merge the two data.tables
  res <- merge(unique(estObj[, ..keepCols]), flatBV, by = "fishId", all.x = TRUE)



  return(res)
}



flattenBVData <- function(BVdata) {
  
  #print all unique BVvalUnitScale values
  uniqueScales <- unique(BVdata$BVvalUnitScale)
  expectedScales <- c("Ageyear", "Lengthmm", "Weightg", "NotApplicable", "Sex", "SMSF", NA)
  
  if(!all(uniqueScales %in% expectedScales)) {
    stop(paste0("Unexpected BVvalUnitScale values found: ",
                   paste(setdiff(uniqueScales, expectedScales), collapse = ", ")))
  }
  

  # Extract AgeYear to age, Lengthmm to lenght and Weightg to weight
  ageData <- BVdata[BVdata$BVvalUnitScale == "Ageyear", .(fishId, age = as.numeric(BVvalueMeas))]
  lengthData <- BVdata[BVdata$BVvalUnitScale == "Lengthmm", .(fishId, length =  as.numeric(BVvalueMeas))]
  weightData <- BVdata[BVdata$BVvalUnitScale == "Weightg", .(fishId, weight = as.numeric(BVvalueMeas))]

  # Merge data.tables
  flatData <- merge(ageData, lengthData, by = "fishId", all = TRUE)
  flatData <- merge(flatData, weightData, by = "fishId", all = TRUE)
  return(flatData)
}

