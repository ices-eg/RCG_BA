# ====================	
# func_localMaxima
# ====================	
	
	# Nuno Prista, 2017-2018
	
	
	# 2018-09-10: "extracted from sample_level_funs1.r"


localMaxima <- function(x) {
 # from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima  
 # Use -Inf instead if x is numeric (non-integer)
   y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}	