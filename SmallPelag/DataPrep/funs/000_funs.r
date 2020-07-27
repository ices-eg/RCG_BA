convert.lon.lat.statsq<-function (lon, lat) 
{
# from package COSTeda
    if (any(lat < 36) | any(lat > 80)) 
        stop("lat value out of range")
    if (any(lon < (-44)) | any(lon > 69)) 
        stop("lon value out of range")
    newlat <- cut(lat, seq(35.5, 80.5, 0.5), right = FALSE)
    newlon <- cut(lon, (-44:69), right = FALSE)
    labels <- 0:90
    latlabels <- ifelse(labels < 10, paste("0", labels, sep = ""), 
        as.character(labels))
    lonlabels <- c("A0", "A1", "A2", "A3", paste(rep(LETTERS[c(2:8, 
        10:12, 13)], rep(10, 11)), rep(0:9, 11), sep = ""))
    y <- latlabels[as.numeric(newlat)]
    x <- lonlabels[as.numeric(newlon)]
    ices <- paste(y, x, sep = "")
    if (any(is.na(x)) | any(is.na(y))) 
        warning("Not all points have been matched to an ICES rectangle.")
    return((ices))
}	