
#' @example
#' beaKey <- 'your bea api key'
#' inds <- shortlist();
#' gdp_san <- beaProdByInd(beaKey, 'gdp_san', indVec = inds);


beaProdByInd <- function(beaKey, component, geofips = 'State', indVec = c()) {

	getList <- indVec;
	requireNamespace('data.table')

	dtList <- lapply(getList[, IndustryID], function(thisInd){
		thisResp <- euroStates::beaGet(
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
