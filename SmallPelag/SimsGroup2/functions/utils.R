
#' Check if a data frame has columns
#'
#'Checks if all of the columns in cols are in the data frame column names
#'
#' @param df  data frame
#' @param cols  column names that the df should contain
#'
#' @return boolean vector containing the presence info for each checked column
#' @export
#'
hascols<-function(df, cols){
  res<-sapply(cols, function(x) 
    if(x %in% colnames(df)) {
        return(TRUE)
    }
    else(return(FALSE))
  )
  
}

#' Capitalize the first letter of a string
#'
#' @param x string
#'
#' @return String
#' @export
#'
up1 <- function(x) {
  x<-tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

date2char <- function(date, sep = ""){
  y <- as.character(lubridate::year(date))
  m <- as.character(lubridate::month(date))
  if(nchar(m)==1){
    m <- paste0("0", m)
  }
  d <- as.character(lubridate::day(date))
  if(nchar(d)==1){
    d <- paste0("0", d)
  }
  paste0(y,m,d, collapse = sep)
  
}

#' Replace NA
#'
#'Replaces the NA fields from column x with values from column y
#'if x is missing the y is renamed
#'
#' @param data data frame
#' @param x column name (or nr) that vaues should be replaced
#' @param y the replacment values column
#'
#' @return
#' @export
#'
#' @examples
replaceIfNA <- function(data, x, y){
  if(is.null(data[y])){stop("Replacement column missing!")}
  
  if(x %in% colnames(data)){
    #replace those that have NA
    data[is.na(data[x]) & !is.na(data[y]), x] <- 
      data[is.na(data[x]) & !is.na(data[y]), y]
  }
  else{
    # just rename it
    data[x] <- data[y]
  }
  #remove the second column
  data[y] <- NULL
  
  return(data)
}



#' Check for NA values in data
#'
#' @param data data frame
#' @param checkCol column name to check for NA's
#' @param printCol column name to report missingness
#' @param str should the result be a string instead of a vector
#'
#' @return vector or string of printCol values where checkCol is NA
#' @export
#'
#' @examples
missingVals <- function(data, checkCol, printCol, str = FALSE){
  if(!all(hascols(data, c(checkCol, printCol)))){
    stop("Columns missing in data!")
  }
  udata <- unique(data[,c(checkCol, printCol)])
  res<-udata[is.na(udata[checkCol]), printCol]
  if(str){
    return(paste0(res, collapse = ", "))
  }
  else{
    return(res)
  }
}


#' Make a list of data table columns
#'
#'Function to extract one row with correct type classes from any data table
#'
#' @param df data frame
#' @param printOut should the result be printed as a string
#'
#' @return an inivisble list object wit NA values
#'
table2List <- function(df, printOut = T){
  #struct to list
  x <-data.frame(type = sapply(df, class))
  x$name<-colnames(df)
  x$val <- paste0("` = ",x$type, "(n)")
  if(printOut){
    cat(paste0("`",x$name, x$val, collapse = ",\n"))
  }
  df[1,] <-NA
  invisible(setNames(as.list(df[1,]), x$name))
} 


#' Make a list of 2 data table colums
#'
#'Function to extract all unique rows with any data frame
#'
#' @param df data frame
#' @param nameCol df column name
#' @param valueCol df column name
#' @param printOut should the result be printed as a string
#'
#' @return an inivisble list object wit NA values
#'
columns2List <- function(df, nameCol, valueCol,  printOut = T){
  x <- unique(df[c(nameCol, valueCol)])
  x$val <- paste0("` = \"",x[,valueCol])
  if(printOut){
    cat(paste0("`",x[[nameCol]], x$val, collapse = "\",\n"))
  }
  invisible(setNames(x[[valueCol]], x[[nameCol]]))
} 

#' Make a data frame of Date Parts
#'
#' @param x - Posixct fromat datetime
#' @param vals date parts to extract
#' @param prefix column name prefix
#'
#' @return adata frame with the extracted parts
#'
#' @examples
date2SeparateValues <- function(x, vals = c("quarter","month", "week" ), prefix = NULL){
  #TODO parse other formats also
  x<-list(lubridate::ymd_hms(x))
  #list of values to create
  res<-sapply(vals, function(v, dateVar){
    fun <- get(v, asNamespace("lubridate"))
    do.call(fun,dateVar)
  }, dateVar = x)
  colnames(res) <- paste0(prefix,up1(colnames(res)))
  as.data.frame(res, stringsAsFactors = F)
}



emptyDF <- function(row){
  as.data.frame(row, stringsAsFactors = FALSE)[FALSE,]
}















