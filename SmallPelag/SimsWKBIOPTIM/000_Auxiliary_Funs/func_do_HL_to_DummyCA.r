		do_HL_to_DummyCA<-function(x, id_vars, lengthClass_var, lengthNumber_var){
			
			# Function to convert an RDB HL format (or other type of HL-like format with multiple individuals aggregated in a row) to a RDB-like CA format [one individual, one row]
			# Nuno Prista, SLU-AQUA/H-LAB, 2018-10-15
			
			# Note:
				# this function took time to build
				
				# if you find that you need to adapt it please inform nuno.prista@slu.se on the need and changes made
				# it is nice to keep track of these issues to keep improving the code
				# and might just be invited to co-author it ;)
				
				# was this function very useful to you? think about emailing the author(s) and telling them that. It will be quite nice to hear that. And you might just get an updates some day ;)
						
			# x is a data.frame containing id_vars, lengthClass_var and lengthNumber_var
			# id_vars is a character vector indicating the column names of variables to be used in constructing the id
			# lengthClass_var is a character unit vector indicating the column name for variable length class
			# lengthNumber_var is a character unit vector indicating the column name for variable numbers at length
			
			print("Converting...")			
			
			x$ID<-apply(x[id_vars],1, paste, collapse=" ") 
			
			for (i in colnames(x)[!colnames(x) %in% c(id_vars, lengthClass_var, lengthNumber_var)])
				{
				if(sum(apply(table(x$ID, x[,i])>0,1,sum)>1)>0) {stop (paste("check: id_vars incomplete"))}
				}	
			
				ls1<-split(x, x$ID) # split data.frame by ID [i.e., sample]
				ls2<- lapply(ls1, function(x){ # the function will repeat lengths according to length frequency generating "dummy" individual fish
					lenCls <- rep(x[[lengthClass_var]], x[[lengthNumber_var]])	
					DummyIndivId<-1:length(lenCls)
					
					x <- data.frame(ID = x$ID[1], unique(x[colnames(x)[!colnames(x) %in% c(lengthClass_var, lengthNumber_var, "ID")]]), DummyIndivId, lenCls, row.names = NULL)
					x
					})
			ca_dummy <- do.call("rbind",ls2)
			
			# final check
				print("Quality checking...")
				if(nrow(ca_dummy)!=sum(x[[lengthNumber_var]])) stop ("check: results not consistent - there is an error som")
				print("ok!")
			
			ca_dummy	
		}
