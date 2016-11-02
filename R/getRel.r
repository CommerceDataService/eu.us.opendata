#' Return data from the eurostat and bea APIs
#' 
#' @param term 	 ID of the statistic requested or, if lucky = TRUE, string to search for and return
#' @param lucky 	Boolean operator - if FALSE, must pass relationship ID (see listRel and searchRel)
#' @param beaKey 	Character string representation of user's 36-digit BEA API key. 
#' @return By default, an object of class 'data.table'
#' @import RJSDMX data.table RCurl SPARQL
#' @export 

getRel <- function(term = '', lucky = FALSE, beaKey = '') { 
# TODO: Need to replace temporary method for getting list of industries once metadata repository has more info
# Note: Multiple lines using beaKey here (one for updateCache, one for searchRel)

	`.`							<-	NULL
	Freq							<-	NULL
	SOURCE						<-	NULL
	EU_ID							<-	NULL
	Rel_ID						<-	NULL
	BEA_ID						<-	NULL
	BEA_Geo						<-	NULL
	EU_Geo						<-	NULL
	GEO_NAME					<-	NULL
	Merge_ID					<-	NULL
	Map_Type					<-	NULL
	IndustryID				<-	NULL
	EU_Merge_ID				<-	NULL
	BEA_Merge_ID			<-	NULL
	Source_Component	<-	NULL
	Target_Component	<-	NULL
	DESC <- NULL
	Rel_name <- NULL 
	BEA_Period <- NULL
	EU_Period <- NULL 
	BEA_Unit <- NULL
	EU_Unit <- NULL
	
	requireNamespace('SPARQL', quietly = TRUE)
	requireNamespace('RJSDMX', quietly = TRUE)
	requireNamespace('RCurl', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)
	eu.us.opendata::updateCache();
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
	  luckyRel <- eu.us.opendata::searchRel(term)
	    
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
	 	
	 	data.table::setkey(mrgEU, key = Target_Component)
	 	data.table::setkey(mrgUS, key = Target_Component)
	 	
	 	mrgEU <- unique(
	 		localMrg[
	 			Merge_ID == thisRel[,EU_Merge_ID] & 
	 			Map_Type == '<http://example.org/struc/FunctionalMap>' & 
	 			Source_Component %in% colnames(euData)]
	 	)
	 	
	 	mrgUS <- unique(
	 		localMrg[
	 			Merge_ID == thisRel[,BEA_Merge_ID] & 
	 			Map_Type == '<http://example.org/struc/FunctionalMap>' & 
	 			Source_Component %in% colnames(usData)]
	 	)
	 	
	 	#Rename fields - right now this is an ugly fix because 
	 	# we don't want to have a table with duplicate column names,
	 	# but some EU source columns mapped to two target columns
	 	# Using the colnames() <- c("") approach risks integrity of the DT
	 	temp <- usData[,c(mrgUS$Source_Component),with=FALSE]
	 	#data.table::setnames(temp, c(mrgUS$Source_Component), c(mrgUS$Target_Component))
	 	temp[,SOURCE := 'bea']
	 	colnames(temp) <- c(mrgUS$Target_Component, "SOURCE")
	 	
	 	temp2 <- euData[,c(mrgEU$Source_Component),with=FALSE]
	 	#data.table::setnames(temp2, c(mrgEU$Source_Component), c(mrgEU$Target_Component))
	 	temp2[,SOURCE := 'eurostat']
	 	colnames(temp2) <- c(mrgEU$Target_Component, "SOURCE")
	 	
	 	#merge; fill = TRUE for debugging
	 	mrg <- data.table::rbindlist(list(temp,temp2), use.names = TRUE, fill = TRUE)
	 	
	 	#mrgDescribe <- thisRel[, .(DESC = Rel_name, BEA_Geo, EU_Geo, BEA_Period, EU_Period, Freq, BEA_Unit, EU_Unit)]
	 	#mrg[, c(colnames(mrgDescribe)) := mrgDescribe]
	 	attributes(mrg)$Description <- thisRel

	 	
	 	#replace "United States" in GEO_NAME
	 	

#On second thought, not sure I like this approach
#	 	mrg[, GEO_NAME := gsub('United States', 'US', GEO_NAME, fixed = TRUE)]
	 	
	 	print(paste0("A total of ",nrow(mrg), " records were retrieved."))
	 	print(paste0("EU = ",nrow(temp2), ", US = ",nrow(temp)))
	 	return(mrg)
	 	
  }
	

}
