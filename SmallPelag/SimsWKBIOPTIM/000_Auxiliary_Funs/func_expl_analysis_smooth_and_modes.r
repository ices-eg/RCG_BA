# ====================	
# expl_analysis_smooth_and_modes
# ====================	
	
	# Nuno Prista, 2017-2018
	
	
	# 2018-09-10: "extracted from sample_level_funs1.r" and renamed for consistency
	# 2018-09-10: renaming of outputs
	# 2018-09-10: added argument "variable"
	# 2019-05-23: added argument "include_CV_MWCV_in_title" [adds these values to title]
	# 2019-05-23: adjusted placement of mode indicator 
	

expl_analysis_smooth_and_modes <- function( df0 = df0, variable = "lenCls", samples_to_analyze = samples_to_analyze, smooth_class_span = 2, min_proportion_to_accept_mode = 0.01, save_plot = FALSE, dir_save = "002_Exploratory_analyses\\", file_root = paste("003_mode_detection_", sep=""), include_CV_MWCV_in_title = FALSE)
{
				# added save_plot option
				#browser()
					for (sample_id in samples_to_analyze)
					{
					df1<-df0[df0$sampId == sample_id,]
					sample_threshold_for_modes <- min_proportion_to_accept_mode * nrow(df1)
					# Exploratory checks
						windows(10,7)
						par(mfrow=c(1,2))
							# lengths freq (original and smooth)
								original_freq<-table(factor(df1[[variable]], levels=seq(min(df1[[variable]]), max(df1[[variable]]), by=original_class_span))); original_freq
								tmp.lt<-df1[[variable]]-df1[[variable]]%%smooth_class_span
								smoothed_freq<-table(factor(tmp.lt, levels=seq(min(tmp.lt, na.rm=T), max(tmp.lt, na.rm=T), by=smooth_class_span))); smoothed_freq
							# modes analyses
								original_modes = localMaxima2(as.numeric(table(factor(df1[[variable]], levels=seq(min(df1[[variable]]), max(df1[[variable]]), by=original_class_span)))))
								original_modes_after_threshold<-original_modes[original_modes %in% which(original_freq>sample_threshold_for_modes)]			
								smoothed_modes = localMaxima2(as.numeric(table(factor(tmp.lt, levels=seq(min(tmp.lt, na.rm=T), max(tmp.lt, na.rm=T), by=smooth_class_span)))))
								smoothed_modes_after_threshold<-smoothed_modes[smoothed_modes %in% which(smoothed_freq>sample_threshold_for_modes)]
							# MWCV and CV determination
								if(include_CV_MWCV_in_title) 
									{							
									mwcv1 <- do_MWCV(df1[[variable]])
									mwcv2 <- do_MWCV(df1[[variable]]-df1[[variable]]%%smooth_class_span)
									cv1 <- do_CV_mean(df1[[variable]])
									cv2 <- do_CV_mean(df1[[variable]]-df1[[variable]]%%smooth_class_span)
									}
							# barplots
								if(include_CV_MWCV_in_title) tit <- paste("original freq\n","CV_mean:",cv1,"; MWCV:",mwcv1, sep="") else tit <- original
								a<-barplot(original_freq, las=2, cex.names=1.1, main=tit, las=2, cex.axis=1.1, cex.main=1)
								abline(h=min_proportion_to_accept_mode*nrow(df1), col="red", lty=2)
								text(x = a[original_modes_after_threshold], y = 0, labels = "|", cex=1.5, col="red")
								if(include_CV_MWCV_in_title) tit <- paste("smooth: class span ", smooth_class_span,"\n","CV_mean:",cv2,"; MWCV:",mwcv2, sep="") else tit <- original								
								a<-barplot(smoothed_freq, las=2, cex.names=1.1, main=tit, las=2, cex.axis=1.1, cex.main=1)
								abline(h=min_proportion_to_accept_mode*nrow(df1), col="red", lty=2)
								text(x = a[smoothed_modes_after_threshold], y = 0, labels = "|", cex=1.5, col="red")
								title(df1$sampId[1], outer=T, line=-1)
								if (save_plot==TRUE)
									{
									savePlot(filename = paste(dir_save, file_root, df1$sampId[1],"_",variable,".png", sep=""), type = "png")				
									dev.off()
									} else {
										keyPressed = readkeygraph("[press enter to continue]")
										dev.off()
										}
					}

				}
