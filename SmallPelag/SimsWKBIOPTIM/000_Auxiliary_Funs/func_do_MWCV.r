do_MWCV<-function(x)
{
 x<-data.frame(dummy=rep(1, length(x)), variable = x)
 sigma_i<-sqrt(nrow(x)*as.matrix(prop.table(table(x$variable))*(1-prop.table(table(x$variable)))))
 cv_i <- sigma_i / (nrow(x)*as.matrix(prop.table(table(x$variable))))
 MWCV<-round(sum(sigma_i)/nrow(x)*100,1)
 MWCV
}