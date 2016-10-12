##TODO: 
# • Use "data.table::setnames(eu/usData,...)" with usMerge + euMerge data.tables to rename columns
# • Use data.table::rbindlist(list(euData, usData))


#' Return data from the eurostat and bea APIs
#' 
#' @param term 	 ID of the statistic requested or, if lucky = TRUE, string to search for and return
#' @param lucky 	Boolean operator - if FALSE, must pass relationship ID (see listRel and searchRel)
#' @return By default, an object of class 'data.table'
#' @import beaR RJSDMX data.table
#' @export 

getRel <- function(term = '', lucky = FALSE) { 
#Think we should set validate = TRUE for all requests
	if(class(beaSpec) != 'character'){
		warning('Please specify the ID or of the data you are looking for.')
		return(paste0('Invalid object class passed to beaGet([list of API parameters]): ', class(beaSpec), '. Should be of class "list"'))
	}
	requireNamespace('beaR', quietly = TRUE)
	requireNamespace('RJSDMX', quietly = TRUE)
	requireNamespace('data.table', quietly = TRUE)
	
	localRel <- loadLocalRel()
	localMrg <- loadLocalMerge()
	localStr <- loadLocalStruc()
	if(!lucky){
	
		thisRel <- localRel[Rel_ID == term][1]
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
	 	
	 	
	 	
	 	
	 	
	 	#usSplitLoc <- gregexpr(pattern = '&',  usrid)
	} else {
		message('[Insert lucky search + return method here]')
		return('[Insert lucky search + return method here]')
	}

}
