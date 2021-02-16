do_CV_mean<-function(x){
stand_err_mean_x<-sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
mean_x<-mean(x, na.rm=TRUE)
cv<-stand_err_mean_x/mean_x
round(cv*100,2)
}