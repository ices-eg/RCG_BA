

# Script for getting the data from the data call into the format need for the fishPiSim package

# Input data - where to find
#   All data can be found at https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/Regional%20Coordination%20Meetings%20Baltic%202017/_layouts/15/start.aspx#/2019%20Meeting%20docs/Forms/AllItems.aspx?RootFolder=%2FExternalSites%2Fdatacollection%2FRegional%20coordination%20meetings%202017%2FRegional%20Coordination%20Meetings%20Baltic%202017%2F2019%20Meeting%20docs%2F06%2E%20Data%2FData%20SMALL%20PELAGIC%20IN%20THE%20BALTIC%2FCombined%20Folder&FolderCTID=0x0120005D7119C421D8D64CA2FBF6B6361605E0&View=%7BB54400AA%2D58C7%2D4017%2D83C7%2D2C4CA1934A1E%7D
#   Original data: In the country specific folders 
#   Combined data: data_compiled.Rdata. A combined dataset create by Nuno using the following script 001 & 002 at /RCG_BA/SmallPelag/DataPrep
#               This will be the input data to this script. 
#               (The two scripts can run on national data by modifying ctry in the start /RCG_BA/SmallPelag/DataPrep/002_compile_and_prepare_countries_data.R)

# Output data - data in the right format
#   An example: .....

# Steps in the script
#   1. Setting up - libraries, folders and loading functions
#   2. 

# 1. Setting up ----

path_input_data <- "Q:/dfad/users/kibi/data/RCG/from_share_point/"
path_output_data <- 

# 2. Get input data -----

load(paste0(path_input_data, "data_compiled.Rdata"))

