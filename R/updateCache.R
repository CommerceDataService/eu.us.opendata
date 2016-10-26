#' Update local metadata cache
#' 
#' @param beaKey 	BEA API key (won't be necessary once SPARQL repository has been updated with timestamp)
#' @param force 	Force local cache to update, even if there have been no changes since your last metadata update
#' @return Nothing, but updates local CSVs
#' @import SPARQL data.table
#' @export 

updateCache <- function(beaKey, force = FALSE){
	`.`							<- NULL
 	mtime         	<- NULL
 	Dataset					<- NULL 
 	XMLUpdateDate 	<- NULL
 	JSONUpdateDate	<- NULL 


	requireNamespace('data.table')
	requireNamespace('SPARQL')
	
	if (force){
		updateLocalMerge();
		updateLocalStruc();
		updateLocalRel();
	} else {

## Won't need this until dydra repo updated
	#	endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'

## For now, we'll just update if the earliest updated date in the list of regional datasets has been updated.
		dateDF <- eu.us.opendata::beaGet(
			list(
				'userid' = beaKey,
				'method' = 'getparametervalues', 
				'datasetname' = 'apidatasetmetadata', 
				'parametername' = 'dataset'
			), asList = TRUE, asTable = FALSE, isMeta=T
		);
		if (is.null(dim(dateDF[[1]]))){
			message (dateDF)
			return (dateDF)
		} else {
		#get latest update
			dateDT <- data.table::data.table(dateDF$ParamValue)
			latestUpdate <- as.POSIXct(
				dateDT[
					grep('regional', tolower(Dataset)), 
					max(c(max(JSONUpdateDate), max(XMLUpdateDate)))
				], format = "%Y-%m-%dT%H:%M:%S")
	
	
	
			localMetadataStore <- paste0(.libPaths()[1], '/eu.us.opendata/rawdata')
			dir.create(localMetadataStore, showWarnings = FALSE, recursive = TRUE)
			
			localMetaFiles <- list.files(path = localMetadataStore, full.names = TRUE);
			localMetaFilesTimes <- file.info(localMetaFiles, extra_cols = TRUE)
			localMetaFilesTimes$Dataset <- gsub(
				paste0(localMetadataStore, '/'), 
				'', 
				attributes(localMetaFilesTimes)$row.names, 
				fixed=T
			)
			localMetaMtime <- data.table::as.data.table(localMetaFilesTimes)[, 
				.(
					Dataset = gsub('.csv', '', Dataset, fixed=T), 
					mtime
				)
			][grep('_Table', Dataset, fixed = T)]
			data.table::setkey(localMetaMtime, key = Dataset)
			
			if (dim(localMetaMtime)[1] == 0){
				updateLocalMerge();
				updateLocalStruc();
				updateLocalRel();
			} else {
				oldestLocal <- localMetaMtime[, min(mtime)]
				if (oldestLocal < latestUpdate) {
					updateLocalMerge();
					updateLocalStruc();
					updateLocalRel();
				}
			}
		}
	}
}