#' Pass list of user specifications (including API key) to return data from BEA API.
#' 
#' @param beaKey 	Character string representation of user's 36-digit BEA API key 
#' @param component 	Character string representation of component parameter (see API documentation) 
#' @param geofips 	Geographic FIPS code (see API documentation) 
#' @param indVec 	Character vector containing industryIDs (typically obtained from "shortlist()" function
#' @keywords internal
#' @return By default, an object of class 'data.table' containing BEA product data by industry and region
#' @import data.table
#' @export 


beaProdByInd <- function(beaKey, component, geofips = 'State', indVec = c()) {
	IndustryID <- NULL
	getList <- indVec;
	requireNamespace('data.table', quietly = TRUE)

	dtList <- lapply(getList[, IndustryID], function(thisInd){
		thisResp <- eu.us.opendata::beaGet(
			list(
				'userid' = beaKey, 
				'datasetname' = 'regionalproduct',
				'method' = 'getdata',
				'frequency' = 'a',
				'year' = 'all',
				'component' = component,
				'geofips' = geofips,
				'industryid' = thisInd
			)
		);
		if(class(thisResp)[1] == 'character'){
			thisResp <-  data.table::data.table(data.frame(Code = paste0('No_Data-',thisInd)))
			warning(paste0('No data for IndustryID ', thisInd, ' of component ', component, ' at GeoFips ', geofips))
		}
		return(thisResp);
	});
	
	return(data.table::rbindlist(dtList, use.names = T, fill = T));

}
