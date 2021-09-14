# ====================	
# func_localMaxima2
# ====================	
	
	# Nuno Prista, 2017-2018
	
	
	# 2018-09-10: "extracted from sample_level_funs1.r"

	# note:
		#requires localMaxima

localMaxima2<-function(x){
# Nuno Prista 2017-06-08
# Adaptation of localMaxima to avoid false detection of plateau in upward sequences AND detect all values in plateau if maxima
# looks like a small thing but took over 3 hours of work to get to result...
# based on https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima and https://stackoverflow.com/questions/7509381/identifying-sequences-of-repeated-numbers-in-r
    
    # 2019-12-16: added if-clause to account for samples with one single class
    
    if(length(x)!=1 & length(unique(x))>1){
	b<-cumsum(rle(x)$lengths)[localMaxima(rle(x)$values)]; b
	elements_in_sequence_of_equal_values<-which(rep(rle(x)$lengths >= 2,times = rle(x)$lengths)==TRUE)
	for (i in 1:length(x))
		{
		if(
			x[i] %in% x[b] # i.e., if x[i] is max 
			& i%in% elements_in_sequence_of_equal_values # and i is part of a sequence 
				& !i%in%b) # and i in not yet in the set
				{
				b<-c(b,i)	# adds i to the set
				}
		}
    } else {
            b <- 1:length(x)
            }    
	sort(b)
}
