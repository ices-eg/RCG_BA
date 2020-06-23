

# Script for getting the data from the data call into the format need for the fishPiSim package

# Kirsten Birch Håkansson, DTU Aqua
#   v1: 20200623 - 1st version

# Input data - where to find
#   All data can be found at https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/Regional%20Coordination%20Meetings%20Baltic%202017/_layouts/15/start.aspx#/2019%20Meeting%20docs/Forms/AllItems.aspx?RootFolder=%2FExternalSites%2Fdatacollection%2FRegional%20coordination%20meetings%202017%2FRegional%20Coordination%20Meetings%20Baltic%202017%2F2019%20Meeting%20docs%2F06%2E%20Data%2FData%20SMALL%20PELAGIC%20IN%20THE%20BALTIC%2FCombined%20Folder&FolderCTID=0x0120005D7119C421D8D64CA2FBF6B6361605E0&View=%7BB54400AA%2D58C7%2D4017%2D83C7%2D2C4CA1934A1E%7D
#   Original data: In the country specific folders 
#   Combined data: data_compiled.Rdata. A combined dataset created by Nuno using the following script 001 & 002 at /RCG_BA/SmallPelag/DataPrep
#               This will be the input data to this script. 
#               (The two scripts can run on national data by modifying ctry in the start /RCG_BA/SmallPelag/DataPrep/002_compile_and_prepare_countries_data.R)

# Output data - data in the right format
#   An example: .....

# Steps in the script
#   1. Setting up - libraries, folders and loading functions
#   2. Get input data
#   3. Some corrections, so e.g. the stock codes are correct
#   4. Run the deriveVar to added needed stuff
#   5. Checks
#   6. Further adaptions. Based on the stuff from https://github.com/ices-tools-dev/FishPi2/blob/master/WP3/Code%20to%20create%20demNS%20for%202015%20and%202016%20-%2015.5.2019.r
#   7. Output data - this data set may need to be further minimized, so it runs faster in the simualtions.
#       That will happen in script 103_ based on the overviews from script 102_


# TODO
#   1. compare my combined data set with Nuno's

# 1. Setting up ----

library(dplyr)

path_input_data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"
path_output_data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"

source("SmallPelag/SimsGroup1/1_sim_landings/deriveVarV5.R")

# 2. Get input data -----

load(paste0(path_input_data, "data_compiled.Rdata"))

# 3. Corrections ----

## Correction of area 28 - based on 

dt1 <- mutate(dt1, area = ifelse(!(vslFlgCtry %in% c("LVA", "EST")) & area == "27.3.d.28", "27.3.d.28.2", area))

dt1 <- mutate(dt1, area = ifelse(sppName == "Sprattus sprattus" &  area %in% c("27.3.d.28.1", "27.3.d.28.2"), "27.3.d.28", area))

# 4. Run deriveVar ----

dat <- deriveVar(dt1)
names(dat)

# 5. Checks ----

## Stock - looks ok

no_stock <- summarise(group_by(filter(dat, stock == "no.defined.stock" & sppName %in% c("Clupea harengus", "Sprattus sprattus")), 
                               sppName, area, year, vslFlgCtry), landWt_ton = sum(landWt/1000))

distinct(dat, vslFlgCtry, landCtry)

# 6. Further adaptions ----

dat$landCtryFlag <-dat$landCtry

## LandType is use to evaluating sampling of foreign vessels - a lot of countries do not sample these in the harbours
dat$landType <- "own"
dat$landType[which(dat$landCtryFlag != dat$vslFlgCtry)] <- "foreign"

dat$landTonnage <- dat$landWt / 1000
dat$fleetType <- paste(dat$vslFlgCtry, dat$vslLenCls, sep = "_")

# making a sppType field based on groupings of the ISSCAAP codes
dat$sppType <- "other"

dat$sppType[which(dat$ISSCAAP %in% c(31:34))] <- "demersal"
dat$sppType[which(dat$ISSCAAP %in% c(51:56))] <- "mollusca"
dat$sppType[which(dat$ISSCAAP %in% c(57))] <- "cephalopod"
dat$sppType[which(dat$ISSCAAP %in% c(41:47))] <- "crustacea"
dat$sppType[which(dat$ISSCAAP %in% c(35:37))] <- "pelagic"
dat$sppType[which(dat$ISSCAAP %in% c(38))] <- "elasmobranchs"
dat$sppType[which(dat$ISSCAAP %in% c(22:25))] <- "diadromous"
dat$sppType[which(dat$ISSCAAP %in% c(74:83))] <- "benthos"
dat$sppType[which(dat$ISSCAAP %in% c(91:93))] <- "weed"
dat$sppType[which(dat$ISSCAAP %in% c(11:21))] <- "freshwater"
dat$sppType[which(dat$ISSCAAP %in% c(61:64))] <- "mammals"

# giving each trip a "main" sppType based on the landed weight by sppType code
tripSppTypeWt <-
  tapply(dat$landWt, list(dat$sppType, dat$fishTripId), sum)
tripMaxSppTypeWt <- apply(tripSppTypeWt, 2, max, na.rm = T)
tripMaxSppType <-
  row.names(tripSppTypeWt)[apply(tripSppTypeWt, 2, which.max)]
tripMaxSppTypeChecked <-
  tripMaxSppType[match(dat$fishTripId, names(tripMaxSppTypeWt))]
dat$tripMainSppType <- tripMaxSppTypeChecked

# 7. Output data ----
saveRDS(dat, paste0(path_output_data, "data_prepared_for_fishPi2.rsd"))
