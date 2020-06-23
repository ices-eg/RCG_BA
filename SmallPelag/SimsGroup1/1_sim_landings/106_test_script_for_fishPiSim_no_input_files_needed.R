

# Script for testing the fishPiSim package - this simulates harbour sampling - the script requires two input files, one with
#  scenario setting and one with specific about the sampling units (in this case port) and inclusion in different sampling frames, 
#  these will be create with script 104_.
#  Original test script and an example of the input files can be found in the fishPiSim package https://github.com/ices-tools-dev/FishPi2/tree/master/WP3

# Kirsten Birch Håkansson, DTU Aqua
#   v1: 20200623 - Just a more or less random setup - not using the input files


#--------------------------------------------------
# Set-up
#--------------------------------------------------
# Set path - PUT YOUR OWN PATHS HERE !!!
path.data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"
setwd(path.data)

# load the libraries and functions
library(fishPiSim)
library(survey)
lengthUnique <- function(x){length(unique(x))}

# SAVE LOCATIONS - PUT YOUR OWN PATHS HERE !!!
# location for saved simulation files
simSaveLoc <- "Q:/mynd/kibi/projects_wks_wgs_rcgs/ISSG_small_pelagics_in_the_Baltic/gits/RCG_BA/SmallPelag/SimsGroup1/1_sim_landings/output/test/"
# location for saved simulation plots
plotSaveLoc <- "Q:/mynd/kibi/projects_wks_wgs_rcgs/ISSG_small_pelagics_in_the_Baltic/gits/RCG_BA/SmallPelag/SimsGroup1/1_sim_landings/output/test/"
# location for saved simulation tables?
tableSaveLoc <- "Q:/mynd/kibi/projects_wks_wgs_rcgs/ISSG_small_pelagics_in_the_Baltic/gits/RCG_BA/SmallPelag/SimsGroup1/1_sim_landings/output/test/"

# Definitions of countries, species, areas, and other definitions of interest
ctryOfInterest <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE")
#widerNSea <- c("27.7.d" , "27.4.a" ,"27.4.b", "27.4.c", "27.3.a.20", "27.3.a.21", "27.3.a")
fishOfInterest <- c("SPR", "HER")
#demISSCAAP <- c(31:34)

# What population are we sampling from ?

dat <- readRDS("data_prepared_minimized_for_fishPi2_test.rsd")

# Create the dataframe xx
unique(dat$landCtry)
xx <- subset(dat, !is.na(landCtry) & !is.na(landLoc) & !is.na(landDate))

if (!is.null(xx)) {
  rm(dat)
  gc()
}

#--------------------------------------------------
# Set-up variables for stratification
#--------------------------------------------------

xx$newStrata <- xx$landCtry
table(xx$newStrata)

#--------------------------------------------------
# Scenario settings
#--------------------------------------------------
# How many simulations do you want to run ?
nsim <- 100

# The scenario settings can either be input manually or read from an input file.
# The benefits of using an input file are that a record remains of the input settings, and it is easier for running multiple scenarios sequentially as a loop

################## If MANUALLY inputting the settings:
# Give your simulation a unique name
scenario <- "test"

# What is the total effort
totalEffort <-  600
minorEffortAllocation <- 1

sampleForeignVessels = TRUE


cat(paste("\n\n\n\nStarting set-up of new simulation: ", scenario, "\n"))

# define your domain:
xx$domain <- paste(xx$stock, sep = "-")

# Define your PSU:

myPSU <- paste(xx$landLoc, xx$landDate, sep = "_") # the psu is site X day for the on-shore sampling

unique(xx$landCtry)

# Create strata:
myStratum <- getStratum(xx, stratification = "landCtry", samplingExclusions = c("NA", NA, "NOR"))



# Create Effort allocation:
myEffort <- getEffort(xx, strata = myStratum, strataType = "stratified",  allocateEffortBy = "landWt",
                      PSU = myPSU, totalEffort = 600, psuThreshold = NA, minorEffortAllocation = NA) # The two latter needs to be NA
# If a "none" stratum is missing, then this one don't work - need to hack


# setUpData sets up the data set:
myData <-setUpData(xx, psu="siteXday", psuStratum = myStratum, stratumEffort = myEffort, domains=c("domain"))

# This is the end of the tryCatch function
# }, error=function(e){cat(paste("\n\n\n ERROR in scenario ",scenario," :"),conditionMessage(e), "\n\n\n Skipping to next scenario")})


# set up arrays to hold the results:
myResObj <-setUpSim(nsim, myData$data, simName = scenario)

cat(paste("\nStarting simulation: ", scenario,"\n"))
# run the simulations
results <- runSim(nsim, myData, maxSsuSamp = 2, myResObj, sampleForeign = F, save = TRUE, saveLoc = simSaveLoc, simName = scenario)


# if nsim > 1 then we calculate some summary results
calcs <-calcRes(results, fishOfInterest = fishOfInterest, ctryOfInterest = ctryOfInterest)
#calcs <-calcRes(resObj) # used when re-running a saved simulation


dateTime <- format(Sys.time(), "%H-%M-%S %d_%m_%Y")

cairo_pdf(file = paste(paste(plotSaveLoc,"S",sep = ""),scenario,nsim,"nsim",totalEffort,"effort", dateTime,"by species.pdf", sep = " "),width=8,height=12, onefile = T, fallback_resolution = 200)
par(mfrow=c(2,1))
devPlot(calcs, main=paste(scenario,totalEffort))
biasPlot(calcs)

par(mfrow=c(5,3))
fishHist(calcs,fishOfInterest)
dev.off()

cairo_pdf(file = paste(paste(plotSaveLoc,"S",sep = ""),scenario,nsim,"nsim",totalEffort,"effort", dateTime,"by domain.pdf", sep = " "),width=8,height=12, onefile = T, fallback_resolution = 200)
par(mfrow=c(2,1))
devPlotDom(calcs)
biasPlotDom(calcs)
dev.off()


cat("Deviation and Bias plots complete\n")


# saving PSU per stratum and effort allocation

if (sampleForeignVessels == FALSE) {psuStrat <- data.frame(myData$psuStratumNOwn)}
if (sampleForeignVessels == TRUE) {psuStrat <- data.frame(myData$psuStratumN)}
          effortStrat <- data.frame(myData$stratumEffort)
          psuStrat$effortStrat <- effortStrat[,1]
          psuStrat$scenario <- paste(scenario,"_",totalEffort, sep = "")
          write.table(psuStrat,file = paste(tableSaveLoc,"PSU effort stratum Totals.csv", sep = ""),
                      append = T, sep=',',row.names=T, col.names=F )



#------------------------------------
#saved results

# saving the summary statistics
calcs$scenarioEff <- paste(scenario,totalEffort, sep = "_")
simSummaryResults <- data.frame("scenarioEff" = calcs$scenarioEff,
                                "effort" = totalEffort,
                                "overallPercentDeviation" = calcs$totBiasEst,
                                "overallPercentDeviationCILo" = calcs$totBiasEstCILo,
                                "overallPercentDeviationCIUp" = calcs$totBiasEstCIUp,
                                "overallDevMedian" = calcs$totBiasEstMedian,
                                "PercentdeviationFOI" = calcs$biasFOI,
                                "PercentdeviationFOICiLo" = calcs$biasFOICiLo,
                                "PercentdeviationFOICiUp" = calcs$biasFOICiUp,
                                "totRSEest" = calcs$totRSEest,
                                "RSEsppOInterest" = calcs$RSEsppFOI,
                                "RSEsppOInterestCiLo" = calcs$RSEsppFOIciLo,
                                "RSEsppOInterestCiUp" = calcs$RSEsppFOIciUp,
                                "RSEctryOInterest" = calcs$RSEctryCOI,
                                "RSEctryOInterestCiLo" = calcs$RSEctryCOIciLo,
                                "RSEctryOInterestCiUp" = calcs$RSEctryCOIciUp,
                                "SEsppOInterest" = calcs$SEsppFOI,
                                "meanSampledTrips" = calcs$meanSampledTrips,
                                "coveragePSU" = myData$coveragePSU,
                                "coverageTonnage" = myData$coverageTonnage,
                                "coverageTrips" = myData$coverageTrips,
                                "dateTime" = dateTime)
if(!is.null(fishOfInterest)){ simSummaryResults <- cbind(simSummaryResults,calcs$RSEresultsFOI,calcs$SEresultsFOI)}
if(!is.null(ctryOfInterest)){ simSummaryResults <- cbind(simSummaryResults,calcs$RSEresultsCOI)}


if (!file.exists(paste(tableSaveLoc,"fishpi2 simulation summary species lists.csv", sep = ""))) {
  write.table(simSummaryResults, file=paste(tableSaveLoc,"fishpi2 simulation summary table.csv", sep = ""),
              append = F, sep=',',row.names=F, col.names=T )
} else {
  write.table(simSummaryResults, file=paste(tableSaveLoc,"fishpi2 simulation summary table.csv", sep = ""),
              append = T, sep=',',row.names=F, col.names=F )
}


#-------------------------------------------------------------------------
# saving estimated totals by species for plotting comparative results from different scenarios
simEstimateResults <- data.frame("sppPop" = calcs$sppPop,
                                 "meanSppEst" = calcs$meanSppEst,
                                 "sppCiLo" = calcs$sppCiLo,
                                 "sppCiUp" = calcs$sppCiUp,
                                 "sppSampSize" = calcs$sppSampSize,
                                 "sppBiasEst" = calcs$sppBiasEst,
                                 "RSEest" = calcs$RSEest,
                                 "scenarioEff" = calcs$scenarioEff)

write.table(cbind(sppFAO = rownames(simEstimateResults), simEstimateResults),file = paste(tableSaveLoc,"S",scenario,nsim,"nsim",totalEffort,"effort", dateTime, "output by species",".csv"),
            append = F, sep=',',row.names=F, col.names=T ) # if running for first time: append = F, sep=',',row.names=F, col.names=T

#-------------------------------------------------------------------------
# species lists and area lists for major and minor ports for each scenario

# this gives the total available landWt per species, not what has been sampled
speciesList <-as.data.frame(tapply(xx$landWt, list(xx$sppFAO, xx$landLocType),sum, na.rm=T))
speciesList$scenarioEff <- paste(scenario,totalEffort, sep = "_")
TripSppCount <- t(as.data.frame(results$sppNumber))
MeanTripsPerSppCount <- as.data.frame(rowSums(TripSppCount/nsim))
speciesList$MeanTripsPerSppCount <- MeanTripsPerSppCount

if (!file.exists(paste(tableSaveLoc,"fishpi2 simulation summary species lists.csv", sep = ""))) {
write.table(cbind(sppFAO = rownames(speciesList), speciesList),file=paste(tableSaveLoc,"fishpi2 simulation summary species lists.csv", sep = ""),
            append = F, sep=',',row.names=F, col.names=T )
} else {
write.table(cbind(sppFAO = rownames(speciesList), speciesList),file=paste(tableSaveLoc,"fishpi2 simulation summary species lists.csv", sep = ""),
              append = T, sep=',',row.names=F, col.names=F )
}

# this gives the total available landWt per area, not what has been sampled
areaList <-as.data.frame(tapply(xx$landWt, list(xx$area, xx$landLocType),sum, na.rm=T))
areaList$scenarioEff <- paste(scenario,totalEffort, sep = "_")

if (!file.exists(paste(tableSaveLoc,"fishpi2 simulation summary species lists.csv", sep = ""))) {
  write.table(cbind(area = rownames(areaList), areaList),file=paste(tableSaveLoc, "fishpi2 simulation summary area lists.csv", sep = ""), sep=',',
              append = F, row.names=F, col.names=T )
} else {
  write.table(cbind(area = rownames(areaList), areaList),file=paste(tableSaveLoc, "fishpi2 simulation summary area lists.csv", sep = ""), sep=',',
              append = T, row.names=F, col.names=F)
}



#-------------------------------------------------------------------------------#
# Plot accumulation curves

  cairo_pdf(file = paste(plotSaveLoc,"Simulation accumulation curves for ", scenario , totalEffort,"effort", nsim, "nsim.pdf", sep = ""),width=8,height=12, onefile = T, fallback_resolution = 200)
  accCurves(results, calcs, sppHighlighted = c("COD", "POK","HAD","PLE","SOL","HKE"))
  dev.off()





 #-------------------------------------------------------------------------------#
 # listing ports sampled during the simulations

 portsSampled <- data.frame(rowSums(calcs$portPSUsamp/nsim))

 portsSampledT <- t(portsSampled)
 row.names(portsSampledT) <- paste(scenario, "nsim", nsim, "effort", totalEffort)

 if (!file.exists(paste(tableSaveLoc,"portFrame_sampled.csv", sep = ""))) {
   write.table(portsSampledT,file = paste(tableSaveLoc,"portFrame_sampled.csv", sep = ""),sep=',',
               row.names=T, col.names=T, append=F)
 } else {
   write.table(portsSampledT,file = paste(tableSaveLoc,"portFrame_sampled.csv", sep = ""),sep=',',
               row.names=T, col.names=F, append=T)
 }





 numTripsSampled <- data.frame(rowSums(calcs$portSSUsamp/nsim))
 colnames(numTripsSampled) <- c(paste(scenario, "nsim", nsim, "effort", totalEffort))
 numTripsSampled <- t(numTripsSampled)

 if (!file.exists(paste(tableSaveLoc,"portFrame_sampled_tripCount.csv", sep = ""))) {
   write.table(numTripsSampled,file = paste(tableSaveLoc,"portFrame_sampled_tripCount.csv", sep = ""),sep=',',
               row.names=T, col.names=T, append=F)
 } else {
   write.table(numTripsSampled,file = paste(tableSaveLoc,"portFrame_sampled_tripCount.csv", sep = ""),sep=',',
               row.names=T, col.names=F, append=T)
 }


##### ANNUAL WORK PLAN OUTPUTS #######

#Table 4d for the Annual Work plan
myStratum[myStratum == "none"] <- "zzz"
#number of ports
locationTotals <- (tapply(xx$landLoc,myStratum,lengthUnique))
#number of trips
regLandingsTotals <- (tapply(xx$fishTripId,myStratum,lengthUnique))
#psu totals
psuTotals <- (tapply(myPSU,myStratum,lengthUnique))
#tonnage of FOI landings
tonnageFOITotalPerPort <- as.data.frame((tapply(xx$landTonnage[xx$sppFAO %in% fishOfInterest],xx$landLoc[xx$sppFAO %in% fishOfInterest], sum, na.rm = T)))
tonnageFOITotalPerPort$ports <- row.names(tonnageFOITotalPerPort)
names(tonnageFOITotalPerPort) <- c("tonnageFOI", "ports")
xx$tonnageFOI <- tonnageFOITotalPerPort$tonnageFOI[match(xx$landLoc,tonnageFOITotalPerPort$ports)]
tonnageFOITotals <- (tapply(xx$tonnageFOI,myStratum,sum,na.rm = T))
# own landings
tonnageFOITotalPerPortOWN <- tapply(xx$landTonnage[xx$sppFAO %in% fishOfInterest & xx$landType == "own"],xx$landLoc[xx$sppFAO %in% fishOfInterest & xx$landType == "own"], sum, na.rm = T)
tonnageFOITotalPerPort$tonnageFOIown <- tonnageFOITotalPerPortOWN[match(tonnageFOITotalPerPort$ports,names(tonnageFOITotalPerPortOWN))]
xx$tonnageFOIown <- tonnageFOITotalPerPort$tonnageFOIown[match(xx$landLoc,tonnageFOITotalPerPort$ports)]
tonnageFOITotalsOWN <- (tapply(xx$tonnageFOIown,myStratum,sum,na.rm = T))
# foreign landings
tonnageFOITotalPerPortFOREIGN <- tapply(xx$landTonnage[xx$sppFAO %in% fishOfInterest & xx$landType == "foreign"],xx$landLoc[xx$sppFAO %in% fishOfInterest & xx$landType == "foreign"], sum, na.rm = T)
tonnageFOITotalPerPort$tonnageFOIforeign <- tonnageFOITotalPerPortFOREIGN[match(tonnageFOITotalPerPort$ports,names(tonnageFOITotalPerPortFOREIGN))]
xx$tonnageFOIforeign <- tonnageFOITotalPerPort$tonnageFOIforeign[match(xx$landLoc,tonnageFOITotalPerPort$ports)]
tonnageFOITotalFOREIGN <- (tapply(xx$tonnageFOIforeign,myStratum,sum,na.rm = T))
#year
years <-substr(xx$landDate,1,4)
year <-names(which.max(table(years)))

myEffort2 <- myEffort[-which(names(myEffort) == "none")]
myEffort2["zzz"] <- 0
#mean trips per port per PSU
meanTripsperPSUperPort <- rowSums(calcs$portSSUsamp)/rowSums(calcs$portPSUsamp)
portsPerStratum <- as.data.frame(tapply(xx$landLoc, myStratum, unique))
meanTripsPerPSU <- array(NA,c(length(names(psuTotals)),2))
meanTripsPerPSU[,1] <- names(psuTotals)
for(i in 1:length(names(psuTotals))){
  ports <- portsPerStratum[row.names(portsPerStratum) == names(psuTotals[i]),]
  meanTripsPerPSU[i,2] <- round(mean(meanTripsperPSUperPort[names(meanTripsperPSUperPort) %in% ports[[1]]], na.rm = T),1)
}

#create table
Table4d_simOutput <- as.data.frame(array(NA,dim=c(lengthUnique(myStratum),11)))
Table4d_simOutput$V1 <- names(psuTotals)
Table4d_simOutput$V2 <- year
Table4d_simOutput$V3 <- locationTotals
Table4d_simOutput$V4 <- psuTotals
Table4d_simOutput$V5 <- regLandingsTotals
Table4d_simOutput$V6 <- round(tonnageFOITotals,1)
Table4d_simOutput$V7 <- round(tonnageFOITotalsOWN,1)
Table4d_simOutput$V8 <- round(tonnageFOITotalFOREIGN,1)
Table4d_simOutput$V9 <- psuTotals
Table4d_simOutput$V10 <- myEffort2
Table4d_simOutput$V11 <- meanTripsPerPSU[,2]


colnames(Table4d_simOutput) <- c("Stratum ID code","Reference years","Average number of locations","Average number of site days", "Average number of registered landings",
                                 "Averagetonnage of relevant species", "Average landed tonnage of relevent species of national fleet", "Average landed tonnage of relevent species of foreign fleet",
                                 "Average number of PSU during the reference years", "Planned number of PSUs", "Planned number of fishing trips sampled per PSU")


write.table(Table4d_simOutput,file = paste(tableSaveLoc,"/Sim output Work Plan/Table 4d for ",scenario,"_",totalEffort,".csv", sep = ""),sep=',',row.names=F, col.names=T)


#-------------------------------------------------------------------------
#  Table 4f - expectations for domains


# 1. All domain results combined


simEstimateResultsDomain <- data.frame("meanDomEst" = calcs$meanDomEst,
                                       "domCiLo" = calcs$domCiLo,
                                       "domCiUp" = calcs$domCiUp,
                                       "domainPop" = calcs$domainPop,
                                       "domBiasEst" = calcs$domBiasEst,
                                       "domSampSize" = calcs$domSampSize,
                                       "domSampSizeCiLo" = calcs$domSampSizeCiLo,
                                       "domSampSizeCiUp" = calcs$domSampSizeCiUp)

domSplit  <- as.data.frame(matrix(unlist(strsplit(row.names(simEstimateResultsDomain), "-")), ncol=3, byrow=TRUE))
simEstimateResultsDomain$sppFAO <- domSplit$V1
simEstimateResultsDomain$area <- domSplit$V2
simEstimateResultsDomain$gear <- domSplit$V3

library(fishPiCodes)
sppNames <- data.frame(whatFish(fishOfInterest))
simEstimateResultsDomain$speciesName <- sppNames$English[match(simEstimateResultsDomain$sppFAO,sppNames$Code)]


write.table(cbind(Domain = rownames(simEstimateResultsDomain),simEstimateResultsDomain),file = paste("X:/fishPi2/simulation plots and tables output/Tables Domain/S",scenario,nsim,"nsim",totalEffort,"effort", dateTime, "output by domain",".csv"),
            append = F, sep=',',row.names=F, col.names=T)


# 2. Domain results split by country

xx$myPSU <- myPSU

includedCtry  <- unlist(strsplit(names(myEffort), "_"))
includedCtry <- unique(includedCtry[includedCtry %in% unique(xx$landCtry)])

meanTripsDomCtry <- as.data.frame(array(NA,dim=c(length(row.names(simEstimateResultsDomain)),length(includedCtry))))
meanTripsDomCtry <- cbind(row.names(simEstimateResultsDomain),meanTripsDomCtry)
colnames(meanTripsDomCtry) <- c("Domain",includedCtry)

meanPSUDomCtry <- as.data.frame(array(NA,dim=c(length(row.names(simEstimateResultsDomain)),length(includedCtry))))
meanPSUDomCtry <- cbind(row.names(simEstimateResultsDomain),meanPSUDomCtry)
colnames(meanPSUDomCtry) <- c("Domain",includedCtry)

for(j in 1:length(includedCtry)){

tripsDomCtry <- as.data.frame(array(0,dim=c(length(row.names(simEstimateResultsDomain)),nsim)))
tripsDomCtry <- cbind(includedCtry[j],row.names(simEstimateResultsDomain),tripsDomCtry)

PSUDomCtry <- as.data.frame(array(0,dim=c(length(row.names(simEstimateResultsDomain)),nsim)))
PSUDomCtry <- cbind(includedCtry[j],row.names(simEstimateResultsDomain),PSUDomCtry)

for(i in 1:(nsim)){
sampledRows <- results$sampRowNames[[i]]

tripCountDomCtry <- tapply(xx$fishTripId[row.names(xx) %in% sampledRows & xx$landCtry == includedCtry[j]],
                           list(xx$domain[row.names(xx) %in% sampledRows & xx$landCtry == includedCtry[j]]),lengthUnique)

tripsDomCtry[,i+2] <- tripCountDomCtry[match(tripsDomCtry[,2],names(tripCountDomCtry))]
tripsDomCtry[,i+2][is.na(tripsDomCtry[,i+2])] <- 0
meanTripsDomCtry[,includedCtry[j]] <- rowSums(tripsDomCtry[3:nsim])/nsim

PSUCountDomCtry <- tapply(xx$myPSU[row.names(xx) %in% sampledRows & xx$landCtry == includedCtry[j]],
                           list(xx$domain[row.names(xx) %in% sampledRows & xx$landCtry == includedCtry[j]]),lengthUnique)

PSUDomCtry[,i+2] <- PSUCountDomCtry[match(PSUDomCtry[,2],names(PSUCountDomCtry))]
PSUDomCtry[,i+2][is.na(PSUDomCtry[,i+2])] <- 0
meanPSUDomCtry[,includedCtry[j]] <- rowSums(PSUDomCtry[3:nsim])/nsim

 }
}

library(tidyr)
library(dplyr)
meanTripsDomCtryLong <- gather(meanTripsDomCtry, key = Country, value = meanTrips, 2:9)
meanPSUDomCtryLong <- gather(meanPSUDomCtry, key = Country, value = meanPSU, 2:9)

domCtryOutput <- left_join(meanTripsDomCtryLong,meanPSUDomCtryLong, by=c("Country", "Domain"))

# split the damin name in component parts
domCtryOutput$Domain <- as.character(domCtryOutput$Domain)
domSplit  <- as.data.frame(matrix(unlist(strsplit(domCtryOutput$Domain, "-")), ncol=3, byrow=TRUE))
domCtryOutput$sppFAO <- domSplit$V1
domCtryOutput$area <- domSplit$V2
domCtryOutput$gear <- domSplit$V3

library(fishPiCodes)
sppNames <- data.frame(whatFish(fishOfInterest))
domCtryOutput$speciesName <- NA
domCtryOutput$speciesName <- sppNames$English[match(domCtryOutput$sppFAO,sppNames$Code)]

domCtryOutput$catchFraction <- "Commercial landings"
domCtryOutput$Variable <- ""
domCtryOutput$ReferenceYear <- year

# Calculate total available trips and psu
total_trips_ctry_domain <- data.frame(tapply(xx$fishTripId, list(xx$domain,xx$landCtry), lengthUnique))
total_trips_ctry_domain <- cbind(Domain = row.names(total_trips_ctry_domain),total_trips_ctry_domain)
total_trips_ctry_domainLong <- gather(total_trips_ctry_domain, key = Country, value = totalTrips, 2:length(colnames(total_trips_ctry_domain)))
total_trips_ctry_domainLong[is.na(total_trips_ctry_domainLong)] <- 0


total_PSU_ctry_domain <- data.frame(tapply(myPSU, list(xx$domain,xx$landCtry), lengthUnique))
total_PSU_ctry_domain <- cbind(Domain = row.names(total_PSU_ctry_domain),total_PSU_ctry_domain)
total_PSU_ctry_domainLong <- gather(total_PSU_ctry_domain, key = Country, value = totalPSU, 2:length(colnames(total_PSU_ctry_domain)))
total_PSU_ctry_domainLong[is.na(total_PSU_ctry_domainLong)] <- 0

domCtryOutput <- left_join(domCtryOutput,total_trips_ctry_domainLong, by = c("Domain","Country"))
domCtryOutput <- left_join(domCtryOutput,total_PSU_ctry_domainLong, by = c("Domain","Country"))
domCtryOutput <- domCtryOutput[,c("Country","Domain","area","gear", "sppFAO" , "speciesName",  "catchFraction",
                                  "Variable" ,"ReferenceYear","totalPSU","totalTrips","meanPSU", "meanTrips")]

colnames(domCtryOutput) <- c("MS","Domain","Sub-area/division/sub-division","Metier", "Species FAO code",
                             "Species", "Catch Fraction", "Variable","Reference year",
                             "Total number of PSUs per nation",
                             "Total number of trips to be sampled per nation",
                             "Expected number of PSUs per nation",
                             "Expected number of trips per nation")

write.table(domCtryOutput,file = paste(tableSaveLoc,"/Sim output Work Plan/Table 4f for ",scenario,"_",totalEffort,".csv", sep = ""),sep=',',row.names=F, col.names=T)


print("end")

#}




