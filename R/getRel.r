#' Return data from the eurostat and bea APIs
#' 
#' @param term 	 ID of the statistic requested or, if lucky = TRUE, string to search for and return
#' @param lucky 	Boolean operator - if FALSE, must pass relationship ID (see listRel and searchRel)
#' @param beaKey 	Character string representation of user's 36-digit BEA API key. Won't be necessary if IndustryID to NAICS mapping is in metadata store.
#' @return By default, an object of class 'data.table'
#' @import RJSDMX data.table RCurl SPARQL
#' @export 

getRel <- function(term = '', lucky = FALSE, beaKey = '') { 
# TODO: Need to replace temporary method for getting list of industries once metadata repository has more info
# Note: Multiple lines using beaKey here (one for updateCache, one for searchRel)

	Freq							<-	NULL
	Source						<-	NULL
	EU_ID							<-	NULL
	Rel_ID						<-	NULL
	BEA_ID						<-	NULL
	BEA_Geo						<-	NULL
	GEO_NAME					<-	NULL
	Merge_ID					<-	NULL
	IndustryID				<-	NULL
	EU_Merge_ID				<-	NULL
	BEA_Merge_ID			<-	NULL
	Source_Component	<-	NULL
	
	
	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('RJSDMX', quietly = TRUE)
	requireNamespace('RCurl', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)
	eu.us.opendata::updateCache(beaKey);
	localRel <- eu.us.opendata::loadLocalRel()
	localMrg <- eu.us.opendata::loadLocalMerge()
	localStr <- eu.us.opendata::loadLocalStruc()
	
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
	  luckyRel <- eu.us.opendata::searchRel(term, beaKey = beaKey)
	    
	    #Check if luckyRel yielded any result (data.frame with more than 1 row)
  	  if(nrow(luckyRel)>0){
  	    #if so retrieve relationship of first in line
  	    thisRel <- localRel[Rel_ID ==  luckyRel[1,2]][1]
  	    print(paste0("Top match for '",term,"': ",  luckyRel[1,1],"% = ", luckyRel[1,3]))
  	  } else {
  	    #if not, print no relationship
  	    thisRel <- data.table::data.table(NA,NA)
  	  }
  	 
	}
	
	#Get and merge data if there are any matches
  if(!is.na(thisRel[1,1,with=FALSE])){
		
		#eurids <- strsplit(gsub('ec.europa.eu/eurostat/SDMX/diss-web/rest/data/', '', thisRel[,EU_ID], fixed = TRUE), '/')
		eurid <- gsub('http://ec.europa.eu/eurostat/SDMX/diss-web/rest/data/', '', thisRel[,EU_ID], fixed = TRUE)
		#Flatten it, because there's no metadata here except the col headers
		euData <- data.table::as.data.table(RJSDMX::getTimeSeriesTable('EUROSTAT', eurid))
	 	#euSplitLoc <- gregexpr(pattern = '/',  eurid)
	 	usLines <- strsplit(gsub('http://www.bea.gov/api/data/?', '', thisRel[,BEA_ID], fixed = TRUE), '&')[[1]]
	 	usrids <- gsub('userid=xxx', paste0('userid=', beaKey), tolower(usLines), fixed = TRUE)
	 	if (any(grepl('datasetname=regionalproduct', tolower(usrids), fixed = T))){
	 	#Use "shortlist" duct tape until inds added to metadata store; then we must build new method
	 		inds <- eu.us.opendata::shortlist();
	 		component <- gsub('component=', tolower(usrids[grepl('component=', tolower(usrids), fixed = T)]));

	 		usData <- beaProdByInd(beaKey, component, geofips = thisRel[, BEA_Geo], indVec = inds[, IndustryID]);
	 	
	 	} else {
		 	beaEval <- gsub("=", "'='", paste0("'", paste(usrids,	collapse = "','"), "'"), fixed = TRUE)
		 	#eval(parse(text = paste0("usData <- eu.us.opendata::beaGet(list(", beaEval, ", 'year' = 'all', 'geofips' = '", thisRel[, BEA_Geo], "', 'frequency' = '", substr(thisRel[,Freq], 1, 1), "'), asWide = FALSE)")))
		 	#GeoFips included in bea_id; passing double params (as above) gives error
		 	eval(parse(text = paste0("usData <- eu.us.opendata::beaGet(list(", beaEval, ", 'year' = 'all', 'frequency' = '", substr(thisRel[,Freq], 1, 1), "'), asWide = FALSE)")))
	 	}
	 	
	 	mrgEU <- localMrg[Merge_ID == thisRel[,EU_Merge_ID] & tolower(Source_Component) %in% tolower(colnames(euData))]
	 	mrgUS <- localMrg[Merge_ID == thisRel[,BEA_Merge_ID] & tolower(Source_Component) %in% tolower(colnames(usData))]
	 	
	 	#Rename fields - right now this is an ugly fix because 
	 	# we don't want to have a table with duplicate column names,
	 	# but some EU source columns mapped to two target columns
	 	# Using the colnames() <- c("") approach risks integrity of the DT
	 	temp <- usData[,c(mrgUS$Source_Component),with=FALSE]
	 	#data.table::setnames(temp, c(mrgUS$Source_Component), c(mrgUS$Target_Component))
	 	temp[,Source := 'bea']
	 	colnames(temp) <- c(mrgUS$Target_Component, "Source")
	 	
	 	temp2 <- euData[,c(mrgEU$Source_Component),with=FALSE]
	 	#data.table::setnames(temp2, c(mrgEU$Source_Component), c(mrgEU$Target_Component))
	 	temp2[,Source := 'eurostat']
	 	colnames(temp2) <- c(mrgEU$Target_Component, "Source")
	 	
	 	#merge
	 	mrg <- data.table::rbindlist(list(temp,temp2), use.names = TRUE, fill = FALSE)
	 	
	 	#replace "United States" in GEO_NAME
	 	
	 	mrg[, GEO_NAME := gsub('United States', 'US', GEO_NAME, fixed = TRUE)]
	 	
	 	
	 	print(paste0("A total of ",nrow(mrg), " records were retrieved."))
	 	print(paste0("EU = ",nrow(temp2), ", US = ",nrow(temp)))
	 	return(mrg)
	 	
  }
	

}
