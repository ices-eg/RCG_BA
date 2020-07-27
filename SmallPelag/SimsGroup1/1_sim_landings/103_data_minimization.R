

# Script for minimizing the data set prepared in script 101_ - reducing number of vars and row, so only what is needed is there.

# Kirsten Birch Håkansson, DTU Aqua
#   v1: 20200623 - 1st version. Only for running the test script in the fishPi2 pakage at https://github.com/ices-tools-dev/FishPi2/tree/master/WP3

# Steps in the script
#   1. Setting up - libraries, folders and loading functions
#   2. Get input data
#   3. Minimizing
#   4. Output data


# 1. Setting up ----

library(dplyr)

version <- "test"

path_input_data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"
path_output_data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"

# 2. Get input data -----

dat <- readRDS(paste0(path_input_data, "data_prepared_for_fishPi2.rsd"))
names(dat)

# 3. Minimizing ----

if (version == "test") {
  
  # This is a modification of  https://github.com/ices-tools-dev/FishPi2/blob/master/WP3/Code%20to%20create%20demNS%20for%202015%20and%202016%20-%2015.5.2019.r
  
  fishOfInterest <- c("SPR", "HER")
  ctryOfInterest <- c("DEU", "DNK", "EST", "FIN", "LTU", "LVA", "POL", "SWE")
  #unique(dat$area)
  #widerNSea <- c("27.7.d" , "27.4.a" ,"27.4.b", "27.4.c" , "27.3.a.20" , "27.3.a.21", "27.3.a")
  vars <-  c("vslFlgCtry","vslLenCls","fishTripId","landLoc","rect","area","foCatEu6","sppCode","sppName",   
             "landWt","landDay","landDate","sppFAO","ISSCAAP","landCtry","gear","stock","landType", "vslId")
  
  sppTypeIndex <- which(dat$tripMainSppType == "pelagic")
  dat_1 <- dat[sppTypeIndex,]
  dat_2 <- subset(dat_1, year == "2017")
  
  
  final <- dat_2[, vars]
}


# 4. Output data ----

saveRDS(final, paste0(path_output_data, "data_prepared_minimized_for_fishPi2_", version, ".rsd"))
