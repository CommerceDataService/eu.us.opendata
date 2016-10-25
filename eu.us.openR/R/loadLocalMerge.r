#' Load local merge metadata cache
#' 
#' @keywords internal
#' @return Local cache of merge metadata, as data.table
#' @import data.table
#' @export 

loadLocalMerge <- function(){
#Think we should set validate = TRUE for all requests

	requireNamespace('data.table', quietly = TRUE)

	localPath <- paste0(.libPaths()[1], '/eu.us.openR/rawdata')

	tryCatch({
		localMerge <- data.table::fread(paste0(localPath, '/Merge_Table.csv'));
		return(localMerge);
	}, error = function(e){
		updateLocalMerge();
	}, finally = {
		localMerge <- data.table::fread(paste0(localPath, '/Merge_Table.csv'));
		return(localMerge);
	})
	
}

