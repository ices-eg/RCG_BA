#' Main Simulation Function
#'
#' @param data the input data validated to have all needed columns
#' @param sppSelectMethod  string with the sampling method name
#'  currently the names include: "alternative_1", "alternative_2",
#'  "alternative_3", "altrnative_4"
#' @param refusals string, vector or numeric 
#' @param withinTripSampUnit string default ("haulId") data column name 
#' this is what will be sampled withinTrip, alternative: "fishdayId"
#' @param withinTripSampMethod string default ("all_hauls")
#'  alternatives: "first_haul", "last_haul", "srswor_haul"
#' @param target_spp  vector of speces names to sample (default NULL) 
#' if NULL samples all species
#' @param target_vessels vector of vessel names to sample (default NULL) 
#' if NULL samples all vessels
#' @param sign_prop numeric(default 0.3) for aleternative 3
#' @param subsampleHaulsPerArea logical (default TRUE) subsample hauls by ICES 
#' subdivision	
#' @param nHaulsMaxPerArea numeric (default 2)
#' @param keep_areas vector of area names (default NULL) 
#' if NULL all areas (ICES subdivisions) are kept
#' @param n_sims numeric (default 1) how many simulations to run
#' @param n_vessels numeric n-vessels per week, week as strata default(5)
#' @param fill_quota logical quota sampling 
#' if FALSE (default) - contacts n_vessels(some many not be fishing) 
#' if TRUE - contacts vessels until n_vessels are found to be fishing
#' @param print_tables logical shoud the simulation results be printed to screen 
#'
#' @return simulation results as a named list of data.tables
#' @export
simulateSampling <- function(data,
                             sppSelectMethod,
                             refusals = "none",
                             withinTripSampUnit = "haulId",
                             withinTripSampMethod = "all_hauls",
                             target_spp = NULL,
                             target_vessels = NULL,
                             sign_prop = 0.3,
                             subsampleHaulsPerArea = TRUE,
                             nHaulsMaxPerArea=2,
                             keep_areas = NULL,
                             n_sims=1, n_vessels=5, fill_quota=FALSE,
                             print_tables = T){

  #set.seed(123)
  if(is.null(keep_areas)){
    keep_areas <- unique(data$area)
  }
  
  
  if(is.null(target_vessels)){
    target_vessels <- unique(data$vslId)
  }
  target_list <- target_vessels
  
  
  #use the data table as it is faster thant data.frame 
  #also data.table provides convinient subsetting
  dt0 <- data.table::as.data.table(data)
  
  #dt0 <- add_fish_count(dt0, target_list)
  
  
  
  # start simulation ---------------------------------------------------------
  
  dt_sample <- calcInParallel(n_sims = n_sims, 
                              refusals=refusals,
                              target_list=target_list,
                              dt0,
                              n_vessels,
                              fill_quota,
                              withinTripSampUnit,
                              withinTripSampMethod,
                              sppSelectMethod,
                              sign_prop,
                              target_spp,
                              subsampleHaulsPerArea,
                              nHaulsMaxPerArea
                              )
  

  #end simulation--------------------------------------------------------------
  #return(dt_sample)
  browser()
  # creates useful object (restricts to target area & non-refusals)
  dt_sample<-lapply(dt_sample, function(x, keep_areas){
    droplevels(x[x$area %in% keep_areas & x$refusal==FALSE,])
    }, keep_areas)
  
  # TODO this might me poosible to move before simulation 
  dt_sample <- lapply(dt_sample, add_fish_count, dt0, target_list)
  
  aggregatedTables <- aggregate_results_to_tables(dt_sample,
                                                  print_tables = print_tables)
  
  #res <- generate_aggreagated_tables(dt_sample)
  
  return(aggregatedTables)
}

#' Calculate Simulations in Parallel
#'
#' @param refusals 
#' @param target_list 
#' @param dt0 
#' @param n_vessels 
#' @param fill_quota 
#' @param withinTripSampUnit 
#' @param withinTripSampMethod 
#' @param sppSelectMethod 
#' @param sign_prop 
#' @param target_spp 
#' @param cores2use 
#' @param n_sims 
#'
#' @return
#' @export
#'
#' @examples
calcInParallel <- function(n_sims = 100, ...){
  runParallel <- ifelse(n_sims < 100, F, T)
  
  #no point to simulate if the count is low the overhead is just too big
  if(!runParallel){
    samples<-lapply(1:n_sims, generateSample, ...)
  }
  else{
    #TODO check how to reduce overhead so that it makes sense tu run in parallel
    # should use package parallel and sockets not forking
    #Sockets work on Windows as well unlike forks
    perCore <- 25
    parsims <- ceiling(n_sims / perCore)
    
    coreCount <- parallel::detectCores()
    cl <- parallel::makeCluster(min(coreCount, parsims))
    parallel::clusterEvalQ(cl, {
      require(data.table)
      source("./R/validators.R")
      source("./R/simulators.R")
    })
    
    samples <- parallel::parLapply(cl, 1:parsims, function(x, ...){
     lapply(1:perCore, generateSample, ...)}, ...)
    
    samples<-unlist(samples, recursive = F)
    
    parallel::stopCluster(cl)
  }
  
  
  return(samples)
}


add_fish_count <- function(dt_sample, dt0, target_list, boxWt = 4){
  # adds expected weight (wtInBox) and number (nInBox) of fish in box
  
  samples_her<-dt0[vslId %in% target_list, list(landWt=landWt), list(withinTripSampUnit,sppName)][, list(wtInBox = sum(landWt[sppName=="Clupea harengus"])/sum(landWt)*boxWt), withinTripSampUnit]
  samples_her$sppName<-"Clupea harengus"
  assumed_herWeight<-0.024 # in kg
  samples_her$nInBox<-round(samples_her$wtInBox/assumed_herWeight)
  
  samples_spr<-dt0[vslId %in% target_list, list(landWt=landWt), list(withinTripSampUnit,sppName)][, list(wtInBox = sum(landWt[sppName=="Sprattus sprattus"])/sum(landWt)*boxWt), withinTripSampUnit]
  samples_spr$sppName<-"Sprattus sprattus"
  assumed_sprWeight<-0.008 # in kg
  samples_spr$nInBox<-round(samples_spr$wtInBox/assumed_sprWeight)						
  
  dt_sample$wtInBox<-NA
  dt_sample$wtInBox[dt_sample$sppName=="Clupea harengus"]<-samples_her$wtInBox[match(paste(dt_sample[dt_sample$sppName=="Clupea harengus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Clupea harengus",]$sppName), paste(samples_her$withinTripSampUnit, samples_her$sppName))]
  dt_sample$wtInBox[dt_sample$sppName=="Sprattus sprattus"]<-samples_spr$wtInBox[match(paste(dt_sample[dt_sample$sppName=="Sprattus sprattus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Sprattus sprattus",]$sppName), paste(samples_spr$withinTripSampUnit, samples_spr$sppName))]
  
  dt_sample$nInBox<-NA
  dt_sample$nInBox[dt_sample$sppName=="Clupea harengus"]<-samples_her$nInBox[match(paste(dt_sample[dt_sample$sppName=="Clupea harengus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Clupea harengus",]$sppName), paste(samples_her$withinTripSampUnit, samples_her$sppName))]
  dt_sample$nInBox[dt_sample$sppName=="Sprattus sprattus"]<-samples_spr$nInBox[match(paste(dt_sample[dt_sample$sppName=="Sprattus sprattus",]$withinTripSampUnit, dt_sample[dt_sample$sppName=="Sprattus sprattus",]$sppName), paste(samples_spr$withinTripSampUnit, samples_spr$sppName))]
  
  # adds expected and number of fish sampled from box (nInBoxSampled)
  
  dt_sample$nInBoxSampled<-dt_sample$nInBox
  dt_sample$nInBoxSampled[dt_sample$nInBoxSampled>50]<-50
  
  return(dt_sample)
  
}



 
  
subsample_hauls_per_area <- function(dt_sample2, nHaulsMaxPerArea){
  # subsamples hauls per subdiv within trip sampled
    ls1 <- split(dt_sample2, dt_sample2$fishTripId)
    ls2 <- lapply(ls1, function(x, subsamp1 = nHaulsMaxPerArea){
      y1 <- split(x, x$area)
      y2 <- lapply(y1, function(z, subsamp2 = subsamp1)
      {
        hauls<-unique(z$withinTripSampUnit)
        # subsamples hauls if n hauls > subsamp2
        if (length(hauls)>subsamp2) {
          z<-z[z$withinTripSampUnit %in% sample(hauls, size = subsamp2),]
          }
        
      })
      x <- data.table::rbindlist(y2)
      									
    })
    data.table::rbindlist(ls2)
  }


#' Subsetting of Input Data
#'
#' @param refusals 
#' @param target_list 
#' @param dt0 
#' @param n_vessels 
#' @param fill_quota 
#' @param withinTripSampUnit 
#' @param withinTripSampMethod 
#' @param sppSelectMethod 
#' @param sign_prop 
#' @param target_spp 
#'
#' @return
generateSample <- function(nr, 
                           refusals,
                           target_list,
                           dt0,
                           n_vessels,
                           fill_quota,
                           withinTripSampUnit,
                           withinTripSampMethod,
                           sppSelectMethod,
                           sign_prop,
                           target_spp,
                           subsampleHaulsPerArea,
                           nHaulsMaxPerArea){

  
  refusals <- calc_refusals(refusals, target_list)
  
  # selection of the vessels
  list_weeks<-unique(dt0$depWeek)[order(unique(dt0$depWeek))]
  out<-data.frame()
  for (week in list_weeks)
  {
    if(n_vessels > length(target_list)){
      warning(paste("Not enough vessels to sample, setting n_vessels to all:",
                    length(target_list)))
      n_vessels <- length(target_list)
    }
    if(fill_quota==FALSE){selectedVsl<-sample(target_list, size=n_vessels, replace=F)}
    if(fill_quota==TRUE){selectedVsl<-sample(target_list, size=length(target_list), replace=F)}
    out<-rbind(out, data.frame(week, vslId=selectedVsl))
  }
  out$weektrip<-out$week+1   
  
  table(out$vslId, out$week)[,1:12]
  
  # selection of the trips
  # take first trip departing next week
  for (i in 1:nrow(out))
  {
    out$fishTripId[i]<-unique(dt0[dt0$depWeek==out$weektrip[i] & dt0$vslId==out$vslId[i],]$fishTripId)[1]
    #out$withinTripSampUnit[i]<-unique(dt0[fishTripId==out$fishTripId[i],]$withinTripSampUnit)[1]
  }
  if(fill_quota==TRUE) # takes a max of n_vessels
  {
    ls1<-split(out,out$week)
    ls2<-lapply(ls1, function(x){
      x[!is.na(x$fishTripId),][1:min(n_vessels,sum(!is.na(x$fishTripId))),]})
    out<-do.call(rbind,ls2)
    out<-out[!is.na(out$week),]    
  }
  
  selected_haulId <- select_haul(dt0, out, withinTripSampUnit, withinTripSampMethod)
  
  dt_sample <- select_spp_sampling_method(dt0, 
                                          sppSelectMethod,
                                          selected_haulId,
                                          sign_prop,
                                          target_spp)
  
  
  dt_sample$callWeek<-dt_sample$depWeek-1
  
  # highlight refusals
  dt_sample$refusal<-FALSE
  dt_sample$refusal[dt_sample$vslId %in% refusals]<-TRUE
  
  if(!sum(!dt_sample$fishTripId %in% out$fishTripId[!is.na(out$fishTripId)])==0){
    stop("See code!")
  }
  
  if(subsampleHaulsPerArea){
    dt_sample<-subsample_hauls_per_area(dt_sample, nHaulsMaxPerArea)
  }
  
  
  return(dt_sample)
}


#' Generate and Print Result Tables
#'
#' @param dt 
#' @param print_tables 
#'
#' @return
#' @export
#'
#' @examples
aggregate_results_to_tables <- function(dt, print_tables = T){
  # creates results object
  res <- list()
  
  # RESULT1a: No samples per area, quarter and spp
  name <- "haulId_per_AreaQuarterSpp"
  res[[name]]<-sppCalcCI(dt,"year",length, c("area", "landQuarter"),
                         print_tables, name = name)
  
  # RESULT1b: No samples per rectangle, quarter and spp
  name<-"haulId_per_RectQuarterSpp"
  res[[name]]<-sppCalcCI(dt, "year",length, c("rect", "landQuarter"),
                         print_tables, name = name)
  
  # RESULT2: No trips per area, quarter and spp
  name<-"fishTripId_per_AreaQuarterSpp"
  res[[name]]<-sppCalcCI(dt,"fishTripId", length, c("area", "landQuarter"),
                         print_tables, name = name, getUnique = T)
  
  # RESULT3: No vessels per area, quarter and spp
  name<-"fishTripId_per_AreaQuarterSpp"
  res[[name]]<-sppCalcCI(dt,"vslId", length, c("area", "landQuarter"),
                         print_tables, name = name, getUnique = T)
  
  # RESULT4: No fish potentially available per area, quarter and spp				
  name<-"fishInBoxTotal_per_AreaQuarterSpp"
  res[[name]]<-sppCalcCI(dt,"nInBox", sum, c("area", "landQuarter"),
                         print_tables, name = name)
  
  # RESULT5: No fish potentially sampled per area, quarter and spp				
  name<-"fishInBoxSampled_per_AreaQuarterSpp"
  res[[name]]<-sppCalcCI(dt,"nInBoxSampled", sum, c("area", "landQuarter"),
                         print_tables, name = name) 
  return(res)
}


generate_aggreagated_tables <- function(dt_sample){
  res <- list()
  #dt_sample2 <- data.table::as.data.table(dt_sample)
    # adds aggregated results
  res[["aggRes"]]<-dt_sample2[, list(vslIdCount = length(unique(vslId)), fishTripIdCount=length(unique(fishTripId)), haulIdCount=length(unique(withinTripSampUnit)), fishInBoxTotal=sum(nInBox, na.rm=T), fishInBoxSampled=sum(nInBoxSampled, na.rm=T)), c('landQuarter', 'sppName', 'area', 'rect')] [order(landQuarter,sppName,area, rect),]
  res[["aggResArea"]]<-dt_sample2[, list(vslIdCount = length(unique(vslId)), fishTripIdCount=length(unique(fishTripId)), haulIdCount=length(unique(withinTripSampUnit)), fishInBoxTotal=sum(nInBox, na.rm=T), fishInBoxSampled=sum(nInBoxSampled, na.rm=T)), c('landQuarter', 'sppName', 'area')] [order(landQuarter,sppName,area),]
  res[["aggResRect"]]<-dt_sample2[, list(vslIdCount = length(unique(vslId)), fishTripIdCount=length(unique(fishTripId)), haulIdCount=length(unique(withinTripSampUnit)), fishInBoxTotal=sum(nInBox, na.rm=T), fishInBoxSampled=sum(nInBoxSampled, na.rm=T)), c('landQuarter', 'sppName', 'rect')] [order(landQuarter,sppName,rect),]
  return(res)
}



select_haul <- function(dt0, out, withinTripSampUnit, withinTripSampMethod){
  # implements haul selection
  aux<-unique(dt0[dt0$fishTripId %in% out$fishTripId, 
                  c("fishTripId", "withinTripSampUnit")]
              [order(dt0$fishTripId, dt0$withinTripSampUnit),])
  if(withinTripSampMethod == "all_hauls") {
    selected_haulId <- aux$withinTripSampUnit
    }
  if(withinTripSampMethod == "first_haul") {
    selected_haulId <- aux[,list(withinTripSampUnit=head(withinTripSampUnit,1)) ,fishTripId]$withinTripSampUnit
    }
  if(withinTripSampMethod == "last_haul") {
    selected_haulId <- aux[,list(withinTripSampUnit=tail(withinTripSampUnit,1)) ,fishTripId]$withinTripSampUnit
    }
  if(withinTripSampMethod == "srswor_haul") {
    selected_haulId <- aux[,list(withinTripSampUnit=sample(withinTripSampUnit,size=1)) ,fishTripId]$withinTripSampUnit
    }
  
  return(selected_haulId)
}


#' Aggreagate Results and Calculate CI
#'
#' @param name - string name for a print table caption
#' @param res  - list of data tables use for calcuation
#' @param aggVar - column nname for aggreagation
#' @param aggFun - function to used for agregation default lenght can also use 
#' sum and others
#' @param selectVars - values to aggregate by
#' @param print_results - should the results be also printed
#' @param getUnique  - should the unique aggVar selectVarsa combination be 
#' found before agregation (important for getting vesselcounts and tripcounts)
#'
#' @return an aggregated data.table in the long format
#' @export
#'
sppCalcCI <- function(res, aggVar, aggFun = length,
                      selectVars= c("area","landQuarter"),
                      print_results = T, getUnique = F, name =""){
  
  
  aggBy<- c(selectVars, "sppName")
  
  x<-lapply(res, function(x, aggBy, aggFun, getUnique){
    if(getUnique){
      x<-x[!duplicated(x[,c(..selectVars, ..aggVar)])] 
    }
    x[, aggFun(eval(parse(text=aggVar))), by=aggBy]
  }, aggBy, aggFun, getUnique)
  
  y<-data.table::rbindlist(x)
  n<-length(x)
  
  y <-y[, list(avg=mean(V1), sd = sd(V1)), by=aggBy]
  #the sample functtion expectedly produces a normal distribution
  #TODO check with somebody if it is the case
  y$error <- qnorm(0.975)*y$sd/sqrt(n)
  y$CIupper <- round(y$avg + y$error, 1)
  y$CIlower <- round(y$avg - y$error, 1)
  y$sample <- paste0(round(y$avg,1), " (", y$CIlower," - ", y$CIupper,")")
  
  if(print_results){
    species <- unique(y$sppName)
    for(spp in species){
      tbl <- y[y$sppName == spp, ]
      castFormula <- as.formula(paste(selectVars[1] ,"~", selectVars[2]))
      tbl <- data.table::dcast(tbl, castFormula, value.var="sample", drop=c(F,F))
      print(knitr::kable(tbl, caption = paste(name, spp), label=spp ))
    }
    
  }
  return(y)
}




select_spp_sampling_method <- function(dt0agg,
                                       sppSelectMethod,
                                       selected_haulId, 
                                       sign_prop,
                                       target_spp = NULL){
  # pulling original data / implements species sampling alternatives
  
  #method_last_letter should be removed later when methods have proper names
  method_last_letter <- substr(sppSelectMethod,
                               nchar(sppSelectMethod),
                               nchar(sppSelectMethod))
  
   if( method_last_letter %in% c("a","b"))
  {
    stop("Deprecated, use the alternative without the ending letter!")
   }
  
  if(!is.null(target_spp)){
    #maybe target_spp is not a vector
    target_spp <- c(target_spp)
    if(!("all" %in% target_spp)){
      dt0agg <- dt0agg[dt0agg$sppName %in% target_spp, ]
    }
    
  }
  agg_columns <- c('year','vslFlgCtry','vslId','vslLenCls','fishTripId',
                   'withinTripSampUnit','depDate','depLoc','arrDate',
                   'arrLoc','landDate','landLoc','rect','area','foCatEu6',
                   'sppCode','sppName','stockCode','depQuarter','depMonth',
                   'depWeek','arrQuarter','arrMonth','arrWeek','landQuarter',
                   'landMonth','landWeek') 
  
 
  dt0agg<-dt0agg[, list(landWt=sum(dt0agg$landWt)), agg_columns]
 
  if(sppSelectMethod=="alternative_1")
  {		
    selected_sample<-dt0agg[withinTripSampUnit %in% selected_haulId,][,list(sppName=sppName[which(landWt==max(landWt))]), list(withinTripSampUnit)]
    
    dt_sample <- dt0agg[paste(withinTripSampUnit, sppName) %in% paste(selected_sample$withinTripSampUnit, selected_sample$sppName),]
    # solves issues of no dominance (randomly samples)
    aux<-tapply(dt_sample$sppName, dt_sample$withinTripSampUnit, function(x){length(unique(x))})
    dt_sample$prob<-dt_sample$withinTripSampUnit %in% names(aux[aux>1])
    ls1<-split(dt_sample, dt_sample$withinTripSampUnit)
    ls2<-lapply(ls1, function(x){
      if (all(x$prob==TRUE)) {x<-x[sample(1:nrow(x), size=1),] } else {x}
    })
    dt_sample<-rbindlist(ls2)									
    # QCA: must yield TRUE if problems solved
    sum(tapply(dt_sample$sppName, dt_sample$withinTripSampUnit, function(x){length(unique(x))})>1)==0
    dt_sample$prob<-NULL
    
    # NOTE: there is a difference between fishTripId present in out and dt_sample 
    #		that is due to vessels not having fished target species
  }
  
  if(sppSelectMethod=="alternative_2")
  {									
    
    dt_sample <- dt0agg[withinTripSampUnit %in% selected_haulId, ]
  }
  
  
  if(sppSelectMethod=="alternative_3")
  {						
      
    dt_sample <- dt0agg[dt0agg$withinTripSampUnit %in% selected_haulId, ]
    # trimming down when rare
    
    aux <- tapply(dt_sample$landWt, list(dt_sample$withinTripSampUnit,dt_sample$sppName), sum)
    aux[is.na(aux)] <- 0
    aux <- prop.table(aux,1)
    aux <- aux > sign_prop
    aux <- melt.array(aux)
    aux <-aux[aux[,3] == TRUE,]
    dt_sample <- dt_sample[paste(dt_sample$withinTripSampUnit, dt_sample$sppName) %in% paste(aux[,1],aux[,2]),]
  }
  
  
  
  if(sppSelectMethod=="alternative_4")
  {	
    
    dt0agg<-dt0agg[, list(landWt=sum(dt0agg$landWt)),agg_columns]
    dt_sample <- dt0agg[withinTripSampUnit %in% selected_haulId, ]
    dim(dt_sample)
    ls1 <- split(dt_sample, dt_sample$withinTripSampUnit)
    ls2 <- lapply(ls1, function(x){
      if (nrow(x)>1) {x<-x[sample(1:nrow(x), size=1),] } else {x}
    })					
    dt_sample<-rbindlist(ls2)
  }
  
  
  return(dt_sample)
}


calc_refusals <- function(refusals, target_list){
  #convert to length if it is a percent
  if (class(refusals)=="numeric") {
    return(sample(target_list, size=ceiling(length(target_list)*refusals/100)))
  }
  refusals
}

melt.array <- function (data, varnames = names(dimnames(data)), na.rm = FALSE, 
          as.is = FALSE, value.name = "value") 
{
  var.convert <- function(x) {
    if (!is.character(x)) 
      return(x)
    x <- type.convert(x, as.is = TRUE)
    if (!is.character(x)) 
      return(x)
    factor(x, levels = unique(x))
  }
  dn <- dimnames(data)
  names(dn) <- varnames
  if (!as.is) {
    dn <- lapply(dn, var.convert)
  }
  labels <- expand.grid(dn, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  if (na.rm) {
    missing <- is.na(data)
    data <- data[!missing]
    labels <- labels[!missing, ]
  }
  value_df <- setNames(data.frame(as.vector(data)), value.name)
  cbind(labels, value_df)
}
