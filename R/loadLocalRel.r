#' Load local relationship metadata cache
#' 
#' @return Local cache of relationship metadata, as data.table
#' @import data.table
#' @export 

loadLocalRel <- function(){
#Think we should set validate = TRUE for all requests

	requireNamespace('data.table', quietly = TRUE)

	localPath <- paste0(.libPaths()[1], '/euroStates/rawdata')

	tryCatch({
		localRel <- data.table::fread(paste0(localPath, '/Relationship_Table.csv'));
		return(localRel);
	}, error = function(e){
		updateLocalRel();
	}, finally = {
		localRel <- data.table::fread(paste0(localPath, '/Relationship_Table.csv'));
		return(localRel);
})

		
}

