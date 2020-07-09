
checkInputDataValidity <- function(testData, withinTripSampUnit = "haulId"){
  testData$withinTripSampUnit<-testData[[withinTripSampUnit]]
  # makes a few consistency checks
  agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId','withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','rect','area','foCatEu6','sppCode','sppName','stockCode','vslId','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
  sum(duplicated(data.frame(testData)[,agg_columns]))
  if (withinTripSampUnit=="haulId" & sum(duplicated(data.frame(testData)[,agg_columns])))stop("Probable error in data format")	
  agg_columns <- c('withinTripSampUnit','depDate','depLoc','arrDate','arrLoc','landDate','landLoc','sppCode','sppName','stockCode','depQuarter','depMonth','depWeek','arrQuarter','arrMonth','arrWeek','landQuarter','landMonth','landWeek')  
  sum(duplicated(data.frame(testData)[,agg_columns]))
  if (withinTripSampUnit=="haulId" & sum(duplicated(data.frame(testData)[,agg_columns])))stop("Probable error in data format")
  
  return(testData)
}	