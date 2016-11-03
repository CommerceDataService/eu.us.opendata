#' Update local metadata cache
#' 
#' @param force 	Force local cache to update, even if there have been no changes since your last metadata update
#' @return Nothing, but updates local CSVs
#' @import SPARQL data.table
#' @export 

updateCache <- function(force = FALSE){
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

		endpt <- 'http://dydra.com/luca-gramaglia/eu-us-partnership-metadata-store/sparql'

		quer <- 'PREFIX pav: <http://purl.org/pav/>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX dct: <http://purl.org/dc/terms/>

SELECT ?ds ?ds_updated WHERE { 
  ?ds a dcat:Catalog .
  ?ds pav:lastUpdateOn ?ds_updated 
}';	
		
		qd <- SPARQL(endpt,quer)
		
		latestUpdate <- as.POSIXct(
			qd$results$ds_updated
			, format = "%Y-%m-%d %H:%M:%S")



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