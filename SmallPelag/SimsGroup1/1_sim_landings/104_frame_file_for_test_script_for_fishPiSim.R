

# Script for creating the two input files required for the test simualtion script, one with
#  scenario setting and one with specific about the sampling units (in this case port) and inclusion in different sampling frames.
#  Original example of the input files can be found in the fishPiSim package https://github.com/ices-tools-dev/FishPi2/tree/master/WP3

# It is probably easier to make in scenario file by hand

# Kirsten Birch Håkansson, DTU Aqua
#   v1: 20200623 - 1st version. Only for running the test script with Baltic SPF data in the fishPi2 pakage at https://github.com/ices-tools-dev/FishPi2/tree/master/WP3

# Steps in the script
#   1. Setting up - libraries, folders and loading functions
#   2. Get input data
#   3. creating the sampling frame file
#   4. Output data


# 1. Setting up ----

library(dplyr)

version <- "test"

path_input_data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"
path_output_data <- "Q:/mynd/kibi/projects_wks_wgs_rcgs/ISSG_small_pelagics_in_the_Baltic/gits/RCG_BA/SmallPelag/SimsGroup1/1_sim_landings/output/test/"

# 2. Get input data -----

dat <- readRDS(paste0(path_input_data, "data_prepared_minimized_for_fishPi2_", version, ".rsd"))
names(dat)




