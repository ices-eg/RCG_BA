
#VN table is the Vessel name table
#the reson for having a VN table is that the raw data has only vessel names
#so the VN is a helpper table to connect the raw data to  the VD table


file2VN <- function(fname,sheet="VN" ){
 colTypes <-c("numeric", "numeric", "text", "text", "text")
 VN <- readxl::read_excel(fname, sheet = sheet, col_types = colTypes)
 return(VN)
 
}

file2VD <- function(fname, year, sheet="VD" ){
  col_types = c("numeric", "text", "numeric", 
                "text", "text", "numeric", "text", 
                "numeric", "numeric", "text",
                 "numeric", "text")
  VD <- readxl::read_excel(fname, sheet = sheet, col_types = col_types)
  VD$VDyear <- year
  
  return(VD)
  
}






