#' Return data from the eurostat and bea APIs
#' 
#' @param term 	 ID of the statistic requested or, if lucky = TRUE, string to search for and return
#' @param lucky 	Boolean operator - if FALSE, must pass relationship ID (see listRel and searchRel)
#' @return By default, an object of class 'data.table'
#' @import beaR RJSDMX data.table
#' @export 

getRel <- function(term = '', lucky = FALSE) { 
#Think we should set validate = TRUE for all requests
# 	if(class(beaSpec) != 'character'){
# 		warning('Please specify the ID or of the data you are looking for.')
# 		return(paste0('Invalid object class passed to beaGet([list of API parameters]): ', class(beaSpec), '. Should be of class "list"'))
# 	}
	requireNamespace('beaR', quietly = TRUE)
	requireNamespace('RJSDMX', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)
	
	localRel <- loadLocalRel()
	localMrg <- loadLocalMerge()
	localStr <- loadLocalStruc()
	
	#Lucky 
	if(!lucky){
	  thisRel <- localRel[Rel_ID == term][1]
	  print(paste0("You have selected '",term,"'"))
	  if(is.na(thisRel[1,1,with=FALSE])){
	    print("However, there are no matches.")
      print("If you have typed in a known Rel_ID, check that the ID matches the Relationship Table exactly.")
      print("Otherwise, for free text search, specify lucky = TRUE")
	  }
	  
	} else {
	  luckyRel <- searchRel(term)
	    
	    #Check if luckyRel yielded any result (data.frame with more than 1 row)
  	  if(nrow(luckyRel)>0){
  	    #if so retrieve relationship of first in line
  	    thisRel <- localRel[Rel_ID ==  luckyRel[1,2]][1]
  	    print(paste0("Top match for '",term,"': ",  luckyRel[1,1],"% = ", luckyRel[1,3]))
  	  } else {
  	    #if not, print no relationship
  	    thisRel <- data.table(NA,NA)
  	  }
  	 
	}
	
	#Get and merge data if there are any matches
  if(!is.na(thisRel[1,1,with=FALSE])){
		
		#eurids <- strsplit(gsub('ec.europa.eu/eurostat/SDMX/diss-web/rest/data/', '', thisRel[,EU_ID], fixed = TRUE), '/')
		eurid <- gsub('ec.europa.eu/eurostat/SDMX/diss-web/rest/data/', '', thisRel[,EU_ID], fixed = TRUE)
		#Flatten it, because there's no metadata here except the col headers
		euData <- as.data.table(RJSDMX::getTimeSeriesTable('EUROSTAT', eurid))
	 	#euSplitLoc <- gregexpr(pattern = '/',  eurid)
	 	usrids <- strsplit(gsub('http://www.bea.gov/api/data/?', '', thisRel[,BEA_ID], fixed = TRUE), '&')[[1]]
	 	
	 	beaEval <- gsub("=", "'='", paste0("'", paste(usrids,	collapse = "','"), "'"), fixed = TRUE)
	 	eval(parse(text = paste0("usData <- beaR::beaGet(list(", beaEval, ", 'year' = 'all', 'geofips' = '", thisRel[, BEA_Geo], "', 'frequency' = '", substr(thisRel[,Freq], 1, 1), "'), asWide = FALSE)")))
	 	
	 	mrgEU <- localMrg[Merge_ID == thisRel[,EU_Merge_ID]]
	 	mrgUS <- localMrg[Merge_ID == thisRel[,BEA_Merge_ID]]
	 	
	 	#Rename fields
	 	temp <- usData[,c(mrgUS$Source_Component),with=FALSE]
	 	colnames(temp)<-c(mrgUS$Target_Component)
	 	
	 	temp2 <- euData[,c(mrgEU$Source_Component),with=FALSE]
	 	colnames(temp2) <- c(mrgEU$Target_Component)
	 	
	 	#merge
	 	mrg <- rbind(temp,temp2)
	 	print(paste0("A total of ",nrow(mrg), " records were retrieved."))
	 	print(paste0("EU = ",nrow(temp2), ", US = ",nrow(temp)))
	 	return(mrg)
	 	
  }
	

}
